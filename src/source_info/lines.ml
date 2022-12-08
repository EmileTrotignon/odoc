let line_directive_parser (line : string) =
  try Scanf.sscanf line "# %i \"%s\"%s" (fun a b _ -> Some (a, b))
  with Scanf.Scan_failure _ -> None

let incr_relative = function
  | Some (i, name) -> Some (i + 1, name)
  | None -> None

let split ~filename src =
  let lines = String.split_on_char '\n' src in
  let relative =
    match filename with Some filename -> Some (1, filename) | None -> None
  in
  let _, poses, _, _ =
    List.fold_left
      (fun (i, poses, count, relative) line ->
        match line_directive_parser line with
        | None ->
            let l = String.length line in
            let new_i, new_pos =
              ( i + 1,
                [ (Types.Line { absolute = i; relative }, (count, count)) ] )
            in
            (new_i, new_pos @ poses, count + l + 1, incr_relative relative)
        | Some _ as relative ->
            let l = String.length line in
            (i, poses, count + l + 1, relative))
      (1, [], 0, relative) lines
  in
  poses
