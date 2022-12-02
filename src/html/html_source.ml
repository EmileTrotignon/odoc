open Odoc_document.Types

let tag_of_token (tok : Parser.token) =
  match tok with
  | WITH -> "WITH"
  | WHILE -> "WHILE"
  | WHEN -> "WHEN"
  | VIRTUAL -> "VIRTUAL"
  | VAL -> "VAL"
  | UNDERSCORE -> "UNDERSCORE"
  | UIDENT _ -> "UIDENT"
  | TYPE -> "TYPE"
  | TRY -> "TRY"
  | TRUE -> "TRUE"
  | TO -> "TO"
  | TILDE -> "TILDE"
  | THEN -> "THEN"
  | STRUCT -> "STRUCT"
  | STRING _ -> "STRING"
  | STAR -> "STAR"
  | SIG -> "SIG"
  | SEMISEMI -> "SEMISEMI"
  | SEMI -> "SEMI"
  | RPAREN -> "RPAREN"
  | REC -> "REC"
  | RBRACKET -> "RBRACKET"
  | RBRACE -> "RBRACE"
  | QUOTED_STRING_ITEM _ -> "QUOTED_STRING_ITEM"
  | QUOTED_STRING_EXPR _ -> "QUOTED_STRING_EXPR"
  | QUOTE -> "QUOTE"
  | QUESTION -> "QUESTION"
  | PRIVATE -> "PRIVATE"
  | PREFIXOP _ -> "PREFIXOP"
  | PLUSEQ -> "PLUSEQ"
  | PLUSDOT -> "PLUSDOT"
  | PLUS -> "PLUS"
  | PERCENT -> "PERCENT"
  | OR -> "OR"
  | OPTLABEL _ -> "OPTLABEL"
  | OPEN -> "OPEN"
  | OF -> "OF"
  | OBJECT -> "OBJECT"
  | NONREC -> "NONREC"
  | NEW -> "NEW"
  | MUTABLE -> "MUTABLE"
  | MODULE -> "MODULE"
  | MINUSGREATER -> "MINUSGREATER"
  | MINUSDOT -> "MINUSDOT"
  | MINUS -> "MINUS"
  | METHOD -> "METHOD"
  | MATCH -> "MATCH"
  | LPAREN -> "LPAREN"
  | LIDENT "failwith" -> "failwith"
  | LIDENT _ -> "LIDENT"
  | LETOP _ -> "LETOP"
  | LET -> "LET"
  | LESSMINUS -> "LESSMINUS"
  | LESS -> "LESS"
  | LBRACKETPERCENTPERCENT -> "LBRACKETPERCENTPERCENT"
  | LBRACKETPERCENT -> "LBRACKETPERCENT"
  | LBRACKETLESS -> "LBRACKETLESS"
  | LBRACKETGREATER -> "LBRACKETGREATER"
  | LBRACKETBAR -> "LBRACKETBAR"
  | LBRACKETATATAT -> "LBRACKETATATAT"
  | LBRACKETATAT -> "LBRACKETATAT"
  | LBRACKETAT -> "LBRACKETAT"
  | LBRACKET -> "LBRACKET"
  | LBRACELESS -> "LBRACELESS"
  | LBRACE -> "LBRACE"
  | LAZY -> "LAZY"
  | LABEL _ -> "LABEL"
  | INT _ -> "INT"
  | INITIALIZER -> "INITIALIZER"
  | INHERIT -> "INHERIT"
  | INFIXOP4 _ -> "INFIXOP4"
  | INFIXOP3 _ -> "INFIXOP3"
  | INFIXOP2 _ -> "INFIXOP2"
  | INFIXOP1 _ -> "INFIXOP1"
  | INFIXOP0 _ -> "INFIXOP0"
  | INCLUDE -> "INCLUDE"
  | IN -> "IN"
  | IF -> "IF"
  | HASHOP _ -> "HASHOP"
  | HASH -> "HASH"
  | GREATERRBRACKET -> "GREATERRBRACKET"
  | GREATERRBRACE -> "GREATERRBRACE"
  | GREATER -> "GREATER"
  | FUNCTOR -> "FUNCTOR"
  | FUNCTION -> "FUNCTION"
  | FUN -> "FUN"
  | FOR -> "FOR"
  | FLOAT _ -> "FLOAT"
  | FALSE -> "FALSE"
  | EXTERNAL -> "EXTERNAL"
  | EXCEPTION -> "EXCEPTION"
  | EQUAL -> "EQUAL"
  | EOL -> "EOL"
  | EOF -> "EOF"
  | END -> "END"
  | ELSE -> "ELSE"
  | DOWNTO -> "DOWNTO"
  | DOTOP _ -> "DOTOP"
  | DOTDOT -> "DOTDOT"
  | DOT -> "DOT"
  | DONE -> "DONE"
  | DOCSTRING _ -> "DOCSTRING"
  | DO -> "DO"
  | CONSTRAINT -> "CONSTRAINT"
  | COMMENT _ -> "COMMENT"
  | COMMA -> "COMMA"
  | COLONGREATER -> "COLONGREATER"
  | COLONEQUAL -> "COLONEQUAL"
  | COLONCOLON -> "COLONCOLON"
  | COLON -> "COLON"
  | CLASS -> "CLASS"
  | CHAR _ -> "CHAR"
  | BEGIN -> "BEGIN"
  | BARRBRACKET -> "BARRBRACKET"
  | BARBAR -> "BARBAR"
  | BAR -> "BAR"
  | BANG -> "BANG"
  | BACKQUOTE -> "BACKQUOTE"
  | ASSERT -> "ASSERT"
  | AS -> "AS"
  | ANDOP _ -> "ANDOP"
  | AND -> "AND"
  | AMPERSAND -> "AMPERSAND"
  | AMPERAMPER -> "AMPERAMPER"

let html_of_doc docs =
  let open Tyxml.Html in
  let a :
      ( [< Html_types.a_attrib ],
        [< Html_types.span_content_fun ],
        [> Html_types.span ] )
      star =
    Unsafe.node "a"
    (* Makes it possible to use <a> inside span. Although this is not standard (see
        https://developer.mozilla.org/en-US/docs/Web/Guide/HTML/Content_categories)
        it is validated by the {{:https://validator.w3.org/nu/#textarea}W3C}. *)
  in
  let rec doc_to_html doc =
    match doc with
    | Source_page.Plain_code s -> txt s
    | Tagged_code (info, docs) -> (
        let children = List.map doc_to_html docs in
        match info with
        | Token tok -> span ~a:[ a_class [ tag_of_token tok ] ] children
        | Line l ->
            span
              ~a:[ a_id (Printf.sprintf "L%d" l); a_class [ "source_line" ] ]
              children
        | Local_jmp (Occurence lbl) -> a ~a:[ a_href ("#def-" ^ lbl) ] children
        | Local_jmp (Def lbl) -> span ~a:[ a_id ("def-" ^ lbl) ] children)
  in
  span ~a:[] @@ List.map doc_to_html docs

let html_of_doc doc =
  Tyxml.Html.pre ~a:[] [ Tyxml.Html.code ~a:[] [ html_of_doc doc ] ]
