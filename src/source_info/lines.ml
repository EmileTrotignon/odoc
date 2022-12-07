let lines_locs src =
  let lines = String.split_on_char '\n' src in
  let is_line_directive line =
    if String.length line > 0 then String.get line 0 = '#' else false
  in
  let _, poses, _ =
    List.fold_left
      (fun (i, poses, count) line ->
        let l = String.length line in
        let new_i, new_pos =
          (* Ignore line directives *)
          if is_line_directive line then (i, poses)
          else (i + 1, [(Types.Line i, (count, count))])
        in
        (new_i, new_pos @ poses, count + l + 1))
      (1, [], 0) lines
  in
  poses

let split src = lines_locs src
(* The order won't matter *)
