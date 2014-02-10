type token =
  | PRAGMA
  | SHARP
  | IDENTIFIER
  | PUNCTUATOR
  | NEW_LINE
  | SECTION
  | STRING of (string)
  | INTEGER of (int)

open Parsing;;
let _ = parse_error;;
# 26 "c2newspeak/pp_parser.mly"

open Pp_syntax
# 17 "c2newspeak/pp_parser.ml"
let yytransl_const = [|
  257 (* PRAGMA *);
  258 (* SHARP *);
  259 (* IDENTIFIER *);
  260 (* PUNCTUATOR *);
  261 (* NEW_LINE *);
  262 (* SECTION *);
    0|]

let yytransl_block = [|
  263 (* STRING *);
  264 (* INTEGER *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\002\000\002\000\004\000\004\000\004\000\
\004\000\004\000\003\000\003\000\000\000"

let yylen = "\002\000\
\004\000\005\000\005\000\002\000\000\000\001\000\001\000\001\000\
\001\000\001\000\002\000\000\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\013\000\000\000\000\000\009\000\010\000\
\008\000\007\000\006\000\000\000\000\000\000\000\000\000\001\000\
\004\000\000\000\000\000\000\000\011\000\003\000\002\000"

let yydgoto = "\002\000\
\004\000\012\000\019\000\013\000"

let yysindex = "\002\000\
\010\255\000\000\255\254\000\000\254\254\003\255\000\000\000\000\
\000\000\000\000\000\000\008\255\254\254\006\255\006\255\000\000\
\000\000\006\255\011\255\012\255\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\013\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\013\255\014\255\014\255\000\000\
\000\000\014\255\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\007\000\249\255\000\000"

let yytablesize = 20
let yytable = "\005\000\
\007\000\008\000\001\000\009\000\010\000\011\000\006\000\020\000\
\014\000\015\000\021\000\003\000\016\000\018\000\000\000\022\000\
\023\000\005\000\012\000\017\000"

let yycheck = "\001\001\
\003\001\004\001\001\000\006\001\007\001\008\001\008\001\015\000\
\006\001\007\001\018\000\002\001\005\001\008\001\255\255\005\001\
\005\001\005\001\005\001\013\000"

let yynames_const = "\
  PRAGMA\000\
  SHARP\000\
  IDENTIFIER\000\
  PUNCTUATOR\000\
  NEW_LINE\000\
  SECTION\000\
  "

let yynames_block = "\
  STRING\000\
  INTEGER\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'pp_token_list) in
    Obj.repr(
# 43 "c2newspeak/pp_parser.mly"
                                        ( Pragma )
# 93 "c2newspeak/pp_parser.ml"
               : Pp_syntax.t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'integer_list) in
    Obj.repr(
# 45 "c2newspeak/pp_parser.mly"
                                        ( Line (_3, _2) )
# 102 "c2newspeak/pp_parser.ml"
               : Pp_syntax.t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'integer_list) in
    Obj.repr(
# 47 "c2newspeak/pp_parser.mly"
                                        ( Non_directive )
# 110 "c2newspeak/pp_parser.ml"
               : Pp_syntax.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'pp_token) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'pp_token_list) in
    Obj.repr(
# 51 "c2newspeak/pp_parser.mly"
                                        ( )
# 118 "c2newspeak/pp_parser.ml"
               : 'pp_token_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 52 "c2newspeak/pp_parser.mly"
                                        ( )
# 124 "c2newspeak/pp_parser.ml"
               : 'pp_token_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 56 "c2newspeak/pp_parser.mly"
                                        ( )
# 131 "c2newspeak/pp_parser.ml"
               : 'pp_token))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 57 "c2newspeak/pp_parser.mly"
                                        ( )
# 138 "c2newspeak/pp_parser.ml"
               : 'pp_token))
; (fun __caml_parser_env ->
    Obj.repr(
# 58 "c2newspeak/pp_parser.mly"
                                        ( )
# 144 "c2newspeak/pp_parser.ml"
               : 'pp_token))
; (fun __caml_parser_env ->
    Obj.repr(
# 59 "c2newspeak/pp_parser.mly"
                                        ( )
# 150 "c2newspeak/pp_parser.ml"
               : 'pp_token))
; (fun __caml_parser_env ->
    Obj.repr(
# 60 "c2newspeak/pp_parser.mly"
                                        ( )
# 156 "c2newspeak/pp_parser.ml"
               : 'pp_token))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : int) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'integer_list) in
    Obj.repr(
# 64 "c2newspeak/pp_parser.mly"
                                        ( )
# 164 "c2newspeak/pp_parser.ml"
               : 'integer_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 65 "c2newspeak/pp_parser.mly"
                                        ( )
# 170 "c2newspeak/pp_parser.ml"
               : 'integer_list))
(* Entry parse *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let parse (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Pp_syntax.t)
