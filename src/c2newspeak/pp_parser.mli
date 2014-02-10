type token =
  | PRAGMA
  | SHARP
  | IDENTIFIER
  | PUNCTUATOR
  | NEW_LINE
  | SECTION
  | STRING of (string)
  | INTEGER of (int)

val parse :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Pp_syntax.t
