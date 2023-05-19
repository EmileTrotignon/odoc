open Lang

type item =
  | CompilationUnit of Compilation_unit.t
  | TypeDecl of TypeDecl.t
  | Module of Module.t
  | Value of Value.t
  | Exception of Exception.t
  | ClassType of ClassType.t
  | Method of Method.t
  | Class of Class.t
  | Extension of Extension.t
  | ModuleType of ModuleType.t
  | Doc of Comment.docs_or_stop

let rec unit ~f acc u =
  let acc = f acc (CompilationUnit u) in
  match u.content with Module m -> signature ~f acc m | Pack _ -> acc

and page idx t =
  let open Page in
  docs idx (`Docs t.content)

and signature ~f acc (s : Signature.t) =
  List.fold_left (signature_item ~f) acc s.items

and signature_item ~f acc s_item =
  match s_item with
  | Module (_, m) -> module_ ~f acc m
  | ModuleType mt -> module_type ~f acc mt
  | ModuleSubstitution _ -> acc
  | ModuleTypeSubstitution _ -> acc
  | Open _ -> acc
  | Type (_, t_decl) -> type_decl ~f acc t_decl
  | TypeSubstitution _ -> acc
  | TypExt te -> type_extension ~f acc te
  | Exception exc -> exception_ ~f acc exc
  | Value v -> value ~f acc v
  | Class (_, cl) -> class_ ~f acc cl
  | ClassType (_, clt) -> class_type ~f acc clt
  | Include i -> include_ ~f acc i
  | Comment d -> docs ~f acc d

and docs ~f acc d = f acc (Doc d)

and include_ ~f acc inc = signature ~f acc inc.expansion.content

and class_type ~f acc ct =
  let acc = f acc (ClassType ct) in
  match ct.expansion with None -> acc | Some cs -> class_signature ~f acc cs

and class_signature ~f acc ct_expr =
  List.fold_left (class_signature_item ~f) acc ct_expr.items

and class_signature_item ~f acc item =
  match item with
  | Method m -> f acc (Method m)
  | InstanceVariable _ -> acc
  | Constraint _ -> acc
  | Inherit _ -> acc
  | Comment d -> docs ~f acc d

and class_ ~f acc cl =
  let acc = f acc (Class cl) in
  match cl.expansion with
  | None -> acc
  | Some cl_signature -> class_signature ~f acc cl_signature

and exception_ ~f acc exc = f acc (Exception exc)

and type_extension ~f acc te = f acc (Extension te)

and value ~f acc v = f acc (Value v)

and module_ ~f acc m =
  let acc = f acc (Module m) in
  match m.type_ with
  | Alias (_, None) -> acc
  | Alias (_, Some s_e) -> simple_expansion ~f acc s_e
  | ModuleType mte -> module_type_expr ~f acc mte

and type_decl ~f acc td = f acc (TypeDecl td)

and module_type ~f acc mt =
  let acc = f acc (ModuleType mt) in
  match mt.expr with
  | None -> acc
  | Some mt_expr -> module_type_expr ~f acc mt_expr

and simple_expansion ~f acc s_e =
  match s_e with
  | Signature sg -> signature ~f acc sg
  | Functor (_, s_e) -> simple_expansion ~f acc s_e

and module_type_expr ~f acc mte =
  match mte with
  | Signature s -> signature ~f acc s
  | Functor (fp, mt_expr) ->
      let acc = functor_parameter ~f acc fp in
      module_type_expr ~f acc mt_expr
  | With { w_expansion = Some sg; _ } -> simple_expansion ~f acc sg
  | TypeOf { t_expansion = Some sg; _ } -> simple_expansion ~f acc sg
  | Path { p_expansion = Some sg; _ } -> simple_expansion ~f acc sg
  | Path { p_expansion = None; _ } -> acc
  | With { w_expansion = None; _ } -> acc
  | TypeOf { t_expansion = None; _ } -> acc

and functor_parameter ~f acc fp =
  match fp with Unit -> acc | Named n -> module_type_expr ~f acc n.expr