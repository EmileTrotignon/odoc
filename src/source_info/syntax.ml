let syntax_highlighting_locs src =
  let lexbuf = Lexing.from_string ~with_positions:true src in
  let rec collect lexbuf =
    let tok = Lexer.token_with_comments lexbuf in
    let loc_start, loc_end = (lexbuf.lex_start_p, lexbuf.lex_curr_p) in
    match tok with
    | EOF -> []
    | COMMENT (_, loc) as tok ->
        (tok, (loc.loc_start.pos_cnum, loc.loc_end.pos_cnum)) :: collect lexbuf
    | tok -> (tok, (loc_start.pos_cnum, loc_end.pos_cnum)) :: collect lexbuf
  in
  collect lexbuf

let highlight src =
  syntax_highlighting_locs src
  |> List.rev_map (fun (x, y) -> (Types.Token x, y))
(* The order won't matter and input can be large *)
