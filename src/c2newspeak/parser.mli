type token =
  | BREAK
  | CONST
  | CONTINUE
  | CASE
  | DEFAULT
  | DO
  | ELSE
  | ENUM
  | STATIC
  | EXTERN
  | FOR
  | IF
  | REGISTER
  | AUTO
  | RETURN
  | VOLATILE
  | SWITCH
  | TYPEDEF
  | WHILE
  | GOTO
  | CHAR
  | DOUBLE
  | FLOAT
  | INT
  | SHORT
  | LONG
  | STRUCT
  | UNION
  | SIGNED
  | UNSIGNED
  | VOID
  | ELLIPSIS
  | COLON
  | COMMA
  | DOT
  | LBRACE
  | RBRACE
  | LBRACKET
  | RBRACKET
  | LPAREN
  | RPAREN
  | NOT
  | EQEQ
  | NOTEQ
  | EQ
  | OREQ
  | SHIFTLEQ
  | SHIFTREQ
  | MINUSEQ
  | PLUSEQ
  | STAREQ
  | DIVEQ
  | MODEQ
  | BXOREQ
  | AMPERSANDEQ
  | SEMICOLON
  | AMPERSAND
  | ARROW
  | AND
  | OR
  | MINUS
  | DIV
  | MOD
  | PLUS
  | MINUSMINUS
  | QMARK
  | PLUSPLUS
  | STAR
  | LT
  | LTEQ
  | GT
  | GTEQ
  | SHIFTL
  | SHIFTR
  | BXOR
  | BOR
  | BNOT
  | ATTRIBUTE
  | EXTENSION
  | VA_LIST
  | CDECL
  | INLINE
  | ASM
  | RESTRICT
  | BUILTIN_CONSTANT_P
  | FUNNAME
  | OFFSETOF
  | SIZEOF
  | TYPEOF
  | EOF
  | NPK of (Csyntax.assertion)
  | SYMBOL of (char)
  | IDENTIFIER of (string)
  | TYPEDEF_NAME of (string)
  | STRING of (string)
  | INTEGER of (string option * string * char option * string option)
  | CHARACTER of (int)
  | FLOATCST of (string * char option)

val parse :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Csyntax.t
val assertion :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Csyntax.assertion
