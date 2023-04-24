open Odoc_model.Lang
open Odoc_model.Paths

let add t q =
  if Identifier.is_internal t.Odoc_model.Index_db.id then q
  else Odoc_model.Index_db.add t q

let rec unit idx t =
  let open Compilation_unit in
  let idx = content idx t.content in
  add { id = (t.id :> Identifier.Any.t); doc = None } idx

and page idx t =
  let open Page in
  docs idx t.content

and content idx =
  let open Compilation_unit in
  function
  | Module m ->
      let idx = signature idx m in
      idx
  | Pack _ -> idx

and signature idx (s : Signature.t) = List.fold_left signature_item idx s.items

and signature_item idx s_item =
  match s_item with
  | Signature.Module (_, m) -> module_ idx m
  | ModuleType mt -> module_type idx mt
  | ModuleSubstitution mod_subst -> module_subst idx mod_subst
  | ModuleTypeSubstitution mt_subst -> module_type_subst idx mt_subst
  | Open _ -> idx
  | Type (_, t_decl) -> type_decl idx t_decl
  | TypeSubstitution t_decl -> type_decl idx t_decl (* TODO check *)
  | TypExt te -> type_extension idx te
  | Exception exc -> exception_ idx exc
  | Value v -> value idx v
  | Class (_, cl) -> class_ idx cl
  | ClassType (_, clt) -> class_type idx clt
  | Include i -> include_ idx i
  | Comment `Stop -> idx
  | Comment (`Docs d) -> docs idx d (* TODO: do not include stopped entries *)

and docs idx d = List.fold_left doc idx d

and doc idx d =
  match d.value with
  | `Paragraph (lbl, _) ->
      add { id = (lbl :> Identifier.Any.t); doc = Some [ d ] } idx
  | `Tag _ -> idx
  | `List (_, ds) ->
      List.fold_left docs idx (ds :> Odoc_model.Comment.docs list)
  | `Heading (_, lbl, _) ->
      add { id = (lbl :> Identifier.Any.t); doc = Some [ d ] } idx
  | `Modules _ -> idx
  | `Code_block (lbl, _, _) ->
      add { id = (lbl :> Identifier.Any.t); doc = Some [ d ] } idx
  | `Verbatim (lbl, _) ->
      add { id = (lbl :> Identifier.Any.t); doc = Some [ d ] } idx
  | `Math_block (lbl, _) ->
      add { id = (lbl :> Identifier.Any.t); doc = Some [ d ] } idx

and include_ idx inc =
  let idx = include_decl idx inc.decl in
  let idx = include_expansion idx inc.expansion in
  idx (* TODO *)

and include_decl idx _decl = idx (* TODO *)

and include_expansion idx expansion = signature idx expansion.content

and class_type idx ct =
  let idx = add { id = (ct.id :> Identifier.Any.t); doc = Some ct.doc } idx in
  let idx = class_type_expr idx ct.expr in
  match ct.expansion with None -> idx | Some cs -> class_signature idx cs

and class_type_expr idx ct_expr =
  match ct_expr with
  | ClassType.Constr (_, _) -> idx
  | ClassType.Signature cs -> class_signature idx cs

and class_signature idx ct_expr =
  List.fold_left class_signature_item idx ct_expr.items

and class_signature_item idx item =
  match item with
  | ClassSignature.Method m ->
      add { id = (m.id :> Identifier.Any.t); doc = Some m.doc } idx
  | ClassSignature.InstanceVariable _ -> idx
  | ClassSignature.Constraint _ -> idx
  | ClassSignature.Inherit _ -> idx
  | ClassSignature.Comment _ -> idx

and class_ idx cl =
  let idx = add { id = (cl.id :> Identifier.Any.t); doc = Some cl.doc } idx in
  let idx = class_decl idx cl.type_ in
  match cl.expansion with
  | None -> idx
  | Some cl_signature -> class_signature idx cl_signature

and class_decl idx cl_decl =
  match cl_decl with
  | Class.ClassType expr -> class_type_expr idx expr
  | Class.Arrow (_, _, decl) -> class_decl idx decl

and exception_ idx exc =
  add { id = (exc.id :> Identifier.Any.t); doc = Some exc.doc } idx

and type_extension idx te =
  match te.constructors with
  | [] -> idx
  | c :: _ ->
      let idx =
        add { id = (c.id :> Identifier.Any.t); doc = Some te.doc } idx
      in
      List.fold_left extension_constructor idx te.constructors

and extension_constructor idx ext_constr =
  add
    { id = (ext_constr.id :> Identifier.Any.t); doc = Some ext_constr.doc }
    idx

and module_subst idx _mod_subst = idx

and module_type_subst idx _mod_subst = idx

and value idx v = add { id = (v.id :> Identifier.Any.t); doc = Some v.doc } idx

and module_ idx m =
  let idx = add { id = (m.id :> Identifier.Any.t); doc = Some m.doc } idx in
  let idx =
    match m.type_ with
    | Module.Alias (_, None) -> idx
    | Module.Alias (_, Some s_e) -> simple_expansion idx s_e
    | Module.ModuleType mte -> module_type_expr idx mte
  in
  idx

and type_decl idx td =
  add { id = (td.id :> Identifier.Any.t); doc = Some td.doc } idx

and module_type idx { id; doc; canonical = _; expr; locs = _ } =
  let idx = add { id = (id :> Identifier.Any.t); doc = Some doc } idx in
  match expr with None -> idx | Some mt_expr -> module_type_expr idx mt_expr

and simple_expansion idx _s_e = idx

and module_type_expr idx mte =
  match mte with
  | ModuleType.Path _ -> idx
  | ModuleType.Signature s -> signature idx s
  | ModuleType.Functor (fp, mt_expr) ->
      let idx = functor_parameter idx fp in
      let idx = module_type_expr idx mt_expr in
      idx
  | ModuleType.With _ -> idx (* TODO *)
  | ModuleType.TypeOf _ -> idx (* TODO *)

and functor_parameter idx fp =
  match fp with
  | FunctorParameter.Unit -> idx
  | FunctorParameter.Named n -> module_type_expr idx n.expr

let compilation_unit u = unit Odoc_model.Index_db.empty u

let page p = page Odoc_model.Index_db.empty p
