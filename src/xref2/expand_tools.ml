type expansion =
  | Signature of Component.Signature.t
  | Functor of Component.FunctorParameter.t * Component.ModuleType.expr

let rec aux_expansion_of_module : Env.t -> Component.Module.t -> expansion =
  let open Component.Module in
  fun env m -> aux_expansion_of_module_decl env m.type_

and aux_expansion_of_module_decl env ty =
  let open Component.Module in
  match ty with
  | Alias path -> aux_expansion_of_module_alias env path
  | ModuleType expr -> aux_expansion_of_module_type_expr env expr

and aux_expansion_of_module_alias env path =
  match Tools.lookup_and_resolve_module_from_path false false env path with
  | Resolved (p, m) -> (
      match aux_expansion_of_module env m, m.doc with
      | Signature sg, [] -> Signature (Strengthen.signature p sg)
      | Signature sg, docs ->
        let sg = Strengthen.signature p sg in
        Signature {sg with items = Comment (`Docs docs) :: sg.items}
      | Functor _ as x, _ -> x )
  | Unresolved p ->
      let err =
        Format.asprintf "Failed to lookup alias module (path=%a) (res=%a)"
          Component.Fmt.module_path path Component.Fmt.module_path p
      in
      failwith err

and aux_expansion_of_module_type_expr env expr : expansion =
  let open Component.ModuleType in
  match expr with
  | Path p -> (
      match Tools.lookup_and_resolve_module_type_from_path false env p with
      | Resolved (_, mt) -> aux_expansion_of_module_type env mt
      | Unresolved p ->
          let p = Component.Fmt.(string_of module_type_path p) in
          failwith (Printf.sprintf "Couldn't find signature: %s" p)
      | exception e ->
        Format.fprintf Format.err_formatter "Failure while looking up path: %a\n%!" Component.Fmt.module_type_path p;
        raise e)
  | Signature s -> Signature (s)
  | With (s, subs) -> (
      let expn = aux_expansion_of_module_type_expr env s in
      match expn with
      | Functor _ -> failwith "This shouldn't be possible!"
      | Signature sg ->
          let sg = Tools.handle_signature_with_subs env sg subs in
          Signature (sg))
  | Functor (arg, expr) -> Functor (arg, expr)
  | TypeOf decl -> aux_expansion_of_module_decl env decl

and aux_expansion_of_module_type env mt =
  let open Component.ModuleType in
  match mt.expr with
  | None -> raise Tools.OpaqueModule
  | Some expr -> aux_expansion_of_module_type_expr env expr

and handle_expansion env id expansion =
  let handle_argument parent arg_opt expr env =
    (* If there's an argument, extend the environment with the argument, then
       do the substitution on the signature to replace the local identifier with
       the global one *)
    match arg_opt with
    | Component.FunctorParameter.Unit -> (env, expr)
    | Named arg ->
        let identifier =
          `Parameter
            ( parent,
              Odoc_model.Names.ParameterName.of_string
                (Ident.Name.module_ arg.Component.FunctorParameter.id) )
        in
        let env' =
          Env.add_module identifier
            (Component.module_of_functor_argument arg)
            env
        in
        let subst =
          Subst.add_module arg.id (`Identifier identifier) Subst.identity
        in
        (env', Subst.module_type_expr subst expr)
  in
  let rec expand id env args expansion =
    match expansion with
    | Signature sg -> (
        match args with
        | [] -> (env, Component.Module.Signature sg)
        | args -> (env, Component.Module.Functor (args, sg)) )
    | Functor (arg, expr) ->
        let env', expr' = handle_argument id arg expr env in
        let res =
          try
            (aux_expansion_of_module_type_expr env' expr')
          with
          | Tools.OpaqueModule -> Signature { items = []; removed = [] }
        in
        expand (`Result id) env' (arg :: args) res
  in
  let env, e = expand id env [] expansion in
  (env, Lang_of.(module_expansion empty id e))

let expansion_of_module_type env id m =
  let open Odoc_model.Paths.Identifier in
  aux_expansion_of_module_type env m
  |> handle_expansion env (id : ModuleType.t :> Signature.t)

let expansion_of_module_type_expr env id expr =
  aux_expansion_of_module_type_expr env expr |> handle_expansion env id

let expansion_of_module env id m =
  let open Odoc_model.Paths.Identifier in
  aux_expansion_of_module env m
  |> handle_expansion env (id : Module.t :> Signature.t)


  exception Clash

  let rec type_expr map t =
    let open Odoc_model.Lang.TypeExpr in
    match t with
    | Var v -> List.assoc v map
    | Any -> Any
    | Alias (t, s) -> if List.mem_assoc s map then raise Clash else Alias (type_expr map t, s)
    | Arrow (l, t1, t2) -> Arrow (l, type_expr map t1, type_expr map t2)
    | Tuple ts -> Tuple (List.map (type_expr map) ts)
    | Constr (p, ts) -> Constr (p, List.map (type_expr map) ts)
    | Polymorphic_variant pv -> Polymorphic_variant (polymorphic_variant map pv)
    | Object o -> Object (object_ map o)
    | Class (path, ts) -> Class (path, List.map (type_expr map) ts)
    | Poly (s, t) -> Poly (s, type_expr map t)
    | Package p -> Package (package map p)
  
  and polymorphic_variant map pv =
    let open Odoc_model.Lang.TypeExpr.Polymorphic_variant in
    let constructor c =
      { c with Constructor.arguments = List.map (type_expr map) c.Constructor.arguments }
    in
    let element = function
      | Type t -> Type (type_expr map t)
      | Constructor c -> Constructor (constructor c)
    in
    { kind = pv.kind
    ; elements = List.map element pv.elements }
  
  and object_ map o =
    let open Odoc_model.Lang.TypeExpr.Object in
    let method_ m =
      { m with type_ = type_expr map m.type_ }
    in
    let field = function
      | Method m -> Method (method_ m)
      | Inherit t -> Inherit (type_expr map t)
    in
    { o with fields = List.map field o.fields }
  
  and package map p =
    let open Odoc_model.Lang.TypeExpr.Package in
    let subst (frag, t) = (frag, type_expr map t) in
    {p with substitutions = List.map subst p.substitutions}
  
  let collapse_eqns eqn1 eqn2 params =
    let open Odoc_model.Lang.TypeDecl in
    let map = List.map2 (fun v p ->
      match v with
      | (Var x, _) -> Some (x, p)
      | (Any , _) -> None)
      eqn2.Equation.params params in
    let map = List.fold_right (fun x xs -> match x with Some x -> x::xs | None -> xs) map [] in
    {eqn1 with Equation.manifest = match eqn2.manifest with | None -> None | Some t -> Some (type_expr map t) }