open Astring
open Odoc_model
open Odoc_model.Names
open Or_error

(*
 * Copyright (c) 2014 Leo White <leo@lpw25.net>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

type parent_spec =
  | Explicit of Paths.Identifier.ContainerPage.t * Lang.Page.child list
  | Package of Paths.Identifier.ContainerPage.t
  | Noparent

type parent_cli_spec =
  | CliParent of string
  | CliPackage of string
  | CliNoparent

let check_is_empty msg = function [] -> Ok () | _ :: _ -> Error (`Msg msg)

(** Used to disambiguate child references. *)
let is_module_name n = String.length n > 0 && Char.Ascii.is_upper n.[0]

(** Accepted child references:

    - [asset-foo] child is an arbitrary asset
    - [module-Foo] child is a module.
    - [module-foo], [Foo] child is a module, for backward compatibility.
    - [page-foo] child is a container or leaf page.
    - [srctree-foo] child is a source tree

  Parses [...-"foo"] as [...-foo] for backward compatibility. *)
let parse_parent_child_reference s =
  let unquote s =
    let len = String.length s in
    if String.head s = Some '"' && String.head ~rev:true s = Some '"' && len > 1
    then String.with_range ~first:1 ~len:(len - 2) s
    else s
  in
  match String.cut ~sep:"-" s with
  | Some ("page", n) -> Ok (Lang.Page.Page_child (unquote n))
  | Some ("srctree", n) -> Ok (Source_tree_child (unquote n))
  | Some ("asset", n) -> Ok (Asset_child (unquote n))
  | Some ("module", n) ->
      Ok (Module_child (unquote (String.Ascii.capitalize n)))
  | Some ("src", _) -> Error (`Msg "Implementation unexpected")
  | Some (k, _) -> Error (`Msg ("Unrecognized kind: " ^ k))
  | None -> if is_module_name s then Ok (Module_child s) else Ok (Page_child s)

let resolve_parent_page resolver f =
  let find_parent = function
    | Lang.Page.Page_child p -> (
        match Resolver.lookup_page resolver p with
        | Some r -> Ok r
        | None -> Error (`Msg "Couldn't find specified parent page"))
    | Source_tree_child _ | Module_child _ | Asset_child _ ->
        Error (`Msg "Expecting page as parent")
  in
  let extract_parent = function
    | { Paths.Identifier.iv = `Page _; _ } as container -> Ok container
    | _ -> Error (`Msg "Specified parent is not a parent of this file")
  in
  parse_parent_child_reference f >>= fun r ->
  find_parent r >>= fun page ->
  extract_parent page.name >>= fun parent ->
  Ok (parent, page.children, page.context)

let parent resolver parent_cli_spec =
  match parent_cli_spec with
  | CliParent f ->
      resolve_parent_page resolver f >>= fun (parent, children, context) ->
      Ok (Explicit (parent, children), context)
  | CliPackage package ->
      Ok
        ( Package (Paths.Identifier.Mk.page (None, PageName.make_std package)),
          None )
  | CliNoparent -> Ok (Noparent, None)

let resolve_imports resolver imports =
  List.map
    (function
      | Lang.Compilation_unit.Import.Resolved _ as resolved -> resolved
      | Unresolved (name, _) as unresolved -> (
          match Resolver.resolve_import resolver name with
          | Some root -> Resolved (root, Names.ModuleName.make_std name)
          | None -> unresolved))
    imports

(** Raises warnings and errors. *)
let resolve_and_substitute ~resolver ~make_root ~hidden ~context
    (parent : Paths.Identifier.ContainerPage.t option) input_file input_type =
  let filename = Fs.File.to_string input_file in
  let unit =
    match input_type with
    | `Cmti ->
        Odoc_loader.read_cmti ~make_root ~parent ~filename
        |> Error.raise_errors_and_warnings
    | `Cmt ->
        Odoc_loader.read_cmt ~make_root ~parent ~filename
        |> Error.raise_errors_and_warnings
    | `Cmi ->
        Odoc_loader.read_cmi ~make_root ~parent ~filename
        |> Error.raise_errors_and_warnings
  in
  let unit = { unit with hidden = hidden || unit.hidden; context } in
  if not unit.Lang.Compilation_unit.interface then
    Printf.eprintf "WARNING: not processing the \"interface\" file.%s\n%!"
      (if not (Filename.check_suffix filename "cmt") then "" (* ? *)
       else
         Printf.sprintf " Using %S while you should use the .cmti file" filename);
  (* Resolve imports, used by the [link-deps] command. *)
  let unit = { unit with imports = resolve_imports resolver unit.imports } in
  let env = Resolver.build_compile_env_for_unit resolver unit in
  let compiled =
    Odoc_xref2.Compile.compile ~filename env unit |> Error.raise_warnings
  in
  (* [expand unit] fetches [unit] from [env] to get the expansion of local, previously
     defined, elements. We'd rather it got back the resolved bit so we rebuild an
     environment with the resolved unit.
     Note that this is bad and once rewritten expand should not fetch the unit it is
     working on. *)
  (*    let expand_env = Env.build env (`Unit resolved) in*)
  (*    let expanded = Odoc_xref2.Expand.expand (Env.expander expand_env) resolved in *)
  compiled

let root_of_compilation_unit ~parent_spec ~hidden ~output ~module_name ~digest =
  let open Root in
  let filename =
    Filename.chop_extension Fs.File.(to_string @@ basename output)
  in
  let result parent =
    let file = Odoc_file.create_unit ~force_hidden:hidden module_name in
    Ok
      {
        id = Paths.Identifier.Mk.root (parent, ModuleName.make_std module_name);
        file;
        digest;
      }
  in
  let check_child = function
    | Lang.Page.Module_child n ->
        String.Ascii.(uncapitalize n = uncapitalize filename)
    | Asset_child _ | Source_tree_child _ | Page_child _ -> false
  in
  match parent_spec with
  | Noparent -> result None
  | Explicit (parent, children) ->
      if List.exists check_child children then result (Some parent)
      else Error (`Msg "Specified parent is not a parent of this file")
  | Package parent -> result (Some parent)

let name_of_output ~prefix output =
  let page_dash_root =
    Filename.chop_extension Fs.File.(to_string @@ basename output)
  in
  String.drop ~max:(String.length prefix) page_dash_root

let page_name_of_output ~is_parent_explicit output =
  let root_name = name_of_output ~prefix:"page-" output in
  (if is_parent_explicit then
     match root_name with
     | "index" ->
         Format.eprintf
           "Warning: Potential name clash - child page named 'index'\n%!"
     | _ -> ());
  root_name

let mld ~parent_spec ~output ~children ~warnings_options ~parent_context input =
  List.fold_left
    (fun acc child_str ->
      match (acc, parse_parent_child_reference child_str) with
      | Ok acc, Ok r -> Ok (r :: acc)
      | Error m, _ -> Error m
      | _, Error (`Msg m) ->
          Error (`Msg ("Failed to parse child reference: " ^ m))
      | _, Error _ -> Error (`Msg "Unknown failure parsing child reference"))
    (Ok []) children
  >>= fun children ->
  let root_name =
    let is_parent_explicit =
      match parent_spec with Explicit _ -> true | _ -> false
    in
    page_name_of_output ~is_parent_explicit output
  in
  let input_s = Fs.File.to_string input in
  let digest = Digest.file input_s in
  let page_name = PageName.make_std root_name in
  let check_child = function
    | Lang.Page.Page_child n -> root_name = n
    | Asset_child _ | Source_tree_child _ | Module_child _ -> false
  in
  (if children = [] then
     (* No children, this is a leaf page. *)
     match parent_spec with
     | Explicit (p, _) -> Ok (Paths.Identifier.Mk.leaf_page (Some p, page_name))
     | Package parent ->
         Ok (Paths.Identifier.Mk.leaf_page (Some parent, page_name))
     | Noparent -> Ok (Paths.Identifier.Mk.leaf_page (None, page_name))
   else
     (* Has children, this is a container page. *)
     let check parents_children v =
       if List.exists check_child parents_children then Ok v
       else Error (`Msg "Specified parent is not a parent of this file")
     in
     (match parent_spec with
     | Explicit (p, cs) ->
         check cs @@ Paths.Identifier.Mk.page (Some p, page_name)
     | Package parent ->
         Ok (Paths.Identifier.Mk.page (Some parent, page_name))
         (* This is a bit odd *)
     | Noparent -> Ok (Paths.Identifier.Mk.page (None, page_name)))
     >>= fun id -> Ok (id :> Paths.Identifier.Page.t))
  >>= fun name ->
  let root =
    let file = Root.Odoc_file.create_page root_name in
    { Root.id = (name :> Paths.Identifier.OdocId.t); file; digest }
  in
  let resolve content =
    let context =
      Some
        {
          Lang.Page.Context.id = (name :> Paths.Identifier.OdocId.t);
          title = Comment.title_of_page content;
          parent_context;
          children = List.map (fun x -> `Unresolved x) children;
        }
    in
    let page =
      Lang.Page.
        { name; root; context; children; content; digest; linked = false }
    in
    Odoc_file.save_page output ~warnings:[] page;
    Ok ()
  in
  Fs.File.read input >>= fun str ->
  Odoc_loader.read_string (name :> Paths.Identifier.LabelParent.t) input_s str
  |> Error.handle_errors_and_warnings ~warnings_options
  >>= function
  | `Stop -> resolve [] (* TODO: Error? *)
  | `Docs content -> resolve content

let handle_file_ext ext =
  match ext with
  | ".cmti" -> Ok `Cmti
  | ".cmt" -> Ok `Cmt
  | ".cmi" -> Ok `Cmi
  | _ ->
      Error (`Msg "Unknown extension, expected one of: cmti, cmt, cmi or mld.")

let compile ~resolver ~parent_cli_spec ~hidden ~children ~output
    ~warnings_options input =
  parent resolver parent_cli_spec >>= fun (parent_spec, parent_context) ->
  let ext = Fs.File.get_ext input in
  if ext = ".mld" then
    mld ~parent_spec ~output ~warnings_options ~children ~parent_context input
  else
    check_is_empty "Not expecting children (--child) when compiling modules."
      children
    >>= fun () ->
    handle_file_ext ext >>= fun input_type ->
    let parent =
      match parent_spec with
      | Noparent -> None
      | Explicit (parent, _) -> Some parent
      | Package parent -> Some parent
    in
    let make_root = root_of_compilation_unit ~parent_spec ~hidden ~output in
    let result =
      Error.catch_errors_and_warnings (fun () ->
          resolve_and_substitute ~resolver ~make_root ~hidden
            ~context:parent_context parent input input_type)
    in
    (* Extract warnings to write them into the output file *)
    let _, warnings = Error.unpack_warnings result in
    Error.handle_errors_and_warnings ~warnings_options result >>= fun unit ->
    Odoc_file.save_unit output ~warnings unit;
    Ok ()
