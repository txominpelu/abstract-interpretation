(*
  C2Newspeak: compiles C code into Newspeak. Newspeak is a minimal language
  well-suited for static analysis.
  Copyright (C) 2007-2011  Charles Hymans, Sarah Zennou

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

  Charles Hymans
  EADS Innovation Works - SE/CS
  12, rue Pasteur - BP 76 - 92152 Suresnes Cedex - France
  email: charles.hymans@penjili.org

  Sarah Zennou
  email: sarah(dot)zennou(at)eads(dot)net
*)


module type T =
sig
  type t = {
    globals: vid list;                     (** program variables *)
    init: blk;                            (** initialization block of globals *)
    fundecs: (fid, fundec) Hashtbl.t;     (** table of all declared functions *)
    src_lang: Newspeak.src_lang;          (** source programming language *)
  }

  and fundec = blk

  and blk = stmt list

  and stmt = stmtkind * Newspeak.location

  and stmtkind =
      Set of (lval * exp)                 (** assignment *)
    | If of (exp * blk * blk)             (** if then else *)
    | While of (exp * blk)                (** while loop *)
    | Call of funexp                      (** function call *)
    | Assert of assertion                 (** assertion *)

  and funexp = FunId of fid

  and lval = Global of vid                (** global variable *)

  and exp =
      Const of cst                        (** integer constant *)
    | Lval of lval                        (** left value *)
    | Random of (integer * integer)       (** random value *)
    | UnOp of (unop * exp)                (** unary operation *)
    | BinOp of (binop * exp * exp)        (** binary operation *)

  and cst = CInt of integer

  and unop = Not                          (** negation *)

  and binop =
      PlusI                               (** addition *)
    | MinusI                              (** substraction *)
    | MultI                               (** multiplication *)
    | DivI                                (** division *)
    | Mod                                 (** modulo *)
    | Gt                                  (** strictly greater than *)
    | Eq                                  (** equality *)

  and bounds = integer * integer

  and assertion = (lval * cmp * cst)      (** x == c
					      x <= c *)
  and cmp =
      Equals
    | IsLess

  and vid = string

  and fid = string

  and integer = Int32.t

  val to_dot : t -> string -> unit

  val to_string: t -> string

  val string_of_unop: unop -> string

  val string_of_binop: binop -> string

  val string_of_loc: Newspeak.location -> string

  val string_of_lval: lval -> string

  val string_of_exp: exp -> string

  val string_of_stmtkind: stmtkind -> string

  val string_of_stmt: stmt -> string

  val string_of_blk: blk -> string

end

type t = {
  globals: vid list;
  init: blk;
  fundecs: (fid, fundec) Hashtbl.t;
  src_lang: Newspeak.src_lang;
}

and fundec = blk

and blk = stmt list

and stmt = stmtkind * Newspeak.location

and stmtkind =
    Set of (lval * exp)
  | If of (exp * blk * blk)
  | While of (exp * blk)
  | Call of funexp
  | Assert of assertion

and funexp = FunId of fid

and lval = Global of vid

and exp =
    Const of cst
  | Lval of lval
  | Random of (integer * integer)
  | UnOp of (unop * exp)
  | BinOp of (binop * exp * exp)

and cst = CInt of integer

and unop = Not

and binop = PlusI | MinusI | MultI | DivI | Mod | Gt | Eq

and bounds = integer * integer

and assertion = (lval * cmp * cst)

and cmp = Equals | IsLess

and vid = string

and fid = string

and integer = Int32.t

let string_of_loc (_, line, _) = string_of_int line

let string_of_cst c =
  match c with
      CInt i -> Int32.to_string i

let string_of_unop op =
  match op with
      Not -> "!"

let string_of_binop op =
  match op with
      PlusI -> "+"
    | MinusI -> "-"
    | MultI -> "*"
    | DivI -> "/"
    | Mod -> "%"
    | Gt -> ">"
    | Eq -> "=="

let string_of_lval lv =
  match lv with
      Global v -> v

let string_of_funexp e =
  match e with
      FunId f -> f

let rec string_of_exp e =
  match e with
      Const c -> string_of_cst c
    | Lval lv -> string_of_lval lv
    | Random (l, u) -> "["^Int32.to_string l^", "^Int32.to_string u^"]"
    | UnOp (op, e) -> string_of_unop op^" "^string_of_exp e
    | BinOp (op, e1, e2) ->
	"("^string_of_exp e1^" "^string_of_binop op^" "^string_of_exp e2^")"

let string_of_cmp x =
  match x with
      Equals -> "=="
    | IsLess -> "<="

let string_of_assertion (lv, cmp, c) =
  string_of_lval lv^" "^string_of_cmp cmp^" "^string_of_cst c

let rec string_of_stmtkind margin x =
  match x with
    | Set (lv, e) -> string_of_lval lv^" = "^string_of_exp e^";"
    | If (e, br1, br2) ->
	"if "^string_of_exp e^" {\n"
	^string_of_blk (margin^"  ") br1
	^"    "^margin^"} else {\n"
	^string_of_blk (margin^"  ") br2
	^"    "^margin^"}"
    | While (e, body) ->
	"while "^string_of_exp e^" {\n"
	^string_of_blk (margin^"  ") body
	^"    "^margin^"}"
    | Call f -> string_of_funexp f^"();"
    | Assert x -> "assert "^string_of_assertion x^";"

and string_of_stmt margin (x, loc) =
  (string_of_loc loc)^": "^margin^string_of_stmtkind margin x

and string_of_blk margin x =
  match x with
      x::tl -> string_of_stmt margin x^"\n"^string_of_blk margin tl
    | [] -> ""

let to_string prog =
  let res = ref "" in
  let string_of_global x = res := !res^"int "^x^";\n" in
  let string_of_fundec f body =
    res := !res^"void "^f^"() {\n";
    res := !res^string_of_blk "  " body;
    res := !res^"}\n"
  in
    List.iter string_of_global (List.rev prog.globals);
    res := !res^string_of_blk "" prog.init;
    Hashtbl.iter string_of_fundec prog.fundecs;
    !res

let string_of_stmtkind = string_of_stmtkind ""

let string_of_stmt = string_of_stmt ""

let string_of_blk = string_of_blk ""

let name_generator =
  let count = ref 0 in
  (function () -> count := !count + 1; Printf.sprintf "%d" !count);;

let rec dot_of_stmtkind blockname stkind current_state =
  match stkind with
    | Set (lv, e) ->
      let next_state = name_generator() in
      let assign = Printf.sprintf "%s -- %s [label=\"%s:=%s\"]" current_state next_state (string_of_lval lv) (string_of_exp e) in
      (next_state, assign)
    | If (e, br1, br2) ->
      let state_br1 = name_generator() in
      let br1_dot = Printf.sprintf "%s -- %s [label=\"%s\"]\n" current_state state_br1 (string_of_exp e) in
      let (br1_develop, last_state_br1) = dot_of_blk "" br1 state_br1 in
      let state_br2 = name_generator() in
      let br2_dot =Printf.sprintf "%s -- %s [label=\"!%s\"]\n" current_state state_br2 (string_of_exp e) in
      let (br2_develop, last_state_br2) = dot_of_blk "" br2 state_br2 in
      let last_state = name_generator() in
      let last_br2 = Printf.sprintf "%s -- %s \n" last_state_br2 last_state in
      let last_br1 = Printf.sprintf "%s -- %s\n" last_state_br1 last_state in
      (last_state, br1_dot ^ br1_develop ^ last_br1 ^ br2_dot ^ br2_develop ^ last_br2)
    | While (e, body) ->
      let  text = "while "^string_of_exp e^" {\n"
	^string_of_blk body
	^"    "^"}" in
      (current_state, text)
    | Call f ->
      let text = string_of_funexp f^"();" in
      (current_state, text)
    | Assert x ->
      let text = "assert "^string_of_assertion x^";" in
      (current_state, text)

and dot_of_stmt blockname (st, loc) = dot_of_stmtkind blockname st

and dot_of_blk blockname block current_state =
  List.fold_left (function (acc, state) -> function st ->
  let (current_state, text) = dot_of_stmt blockname st state in
  (acc ^ (text ^ "\n"), current_state)
) ("",current_state) block

(* Generates a DOT based representation of the control flow graph of
   the program prog and writes it in the file names filename *)
and to_dot prog filename =
  let res = ref "" in
  let dot_of_global x = res := !res^"int "^x^";\n" in
  let dot_of_fundec f body =
    res := !res^"graph "^f^" {\n";
    let name = name_generator() in
    res := !res^ (fst (dot_of_blk "  " body name));
    res := !res^"}\n"
  in
    let initial_name = name_generator() in
    let (graph_dot, _) = dot_of_blk "" prog.init initial_name in
    res := !res^graph_dot;
    Hashtbl.iter dot_of_fundec prog.fundecs;
    (*let fid = open_out filename in
    let prog_dot = "" in*)
    print_string !res;
    Printf.sprintf "graph mygraph { %s } ";
    ()
    (* A compléter *)
    (*close_out fid*)


(* DOT example:
   graph graphname {
     // The label attribute can be used to change the label of a node
     a [label="Foo"];
     // Here, the node shape is changed.
     // These edges both have different line properties
     a -- b -- c [color=blue];
     b -- d [style=dotted];
   }
*)
