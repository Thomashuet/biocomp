{
  open Lexing
  open Parser

  exception Bad_character of char

  let kwd_tbl = ["else", ELSE; "function", FUN; "if", IF; "var", VAR; "while", WHILE]
  let id_or_kwd s = try List.assoc s kwd_tbl with _ -> IDENT s
}

let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']

rule token = parse
| '\n' { new_line lexbuf; token lexbuf }
| ' ' | '\t' { token lexbuf }
| '(' { LP }
| ')' { RP }
| '{' { LB }
| '}' { RB }
| "=" { AFFECT }
| '+' { ADD }
| '-' { SUB }
| '*' { MUL }
| '/' { DIV }
| '%' { MOD }
| "==" { EQ }
| '<' { LT }
| '>' { GT }
| ">=" { GTE }
| "<=" { LTE }
| "!=" { NEQ }
| '!' { NOT }
| "||" { OR }
| "&&" { AND }
| ',' { COMMA }
| ';' { SEMICOLON }
| alpha (alpha | digit | '_')* as id { id_or_kwd id }
| digit+ as n { INT (int_of_string n) }
| '"' { comment lexbuf }
| eof { EOF }
| _ as c { raise (Bad_character c) }

and comment = parse
| '\n' {new_line lexbuf; token lexbuf }
| _ { comment lexbuf }
| eof { EOF }
