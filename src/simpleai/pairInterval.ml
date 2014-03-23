(*
  C2Newspeak: compiles C code into Newspeak. Newspeak is a minimal language
  well-suited for static analysis.
  Copyright (C) 2007  Charles Hymans

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
*)

open UnrelState

module P = Pair
module I = Intervals

type t = P.t * I.t

(*
  - Top (if paridad inf_interval = paridad max_interval) paridad v1, i
  - x (if paridad inf_interval = paridad max_interval) if paridad inf_interval = x  =>      (Even, i) else (Top, i)  - x (if paridad inf_interval <> paridad max_interval) (Top, i)
*)
let parity x = P.singleton x

let reduce p i =
  let open P in
  match (p, i) with
  | (_, I.Val (x, y)) when (parity x) <> (parity y) -> (Top, i)
  | (Top, I.Val (x, y)) when (parity x) = (parity y) -> (parity x, i)
  | (p, I.Val (x, y)) when (parity x) = (parity y) ->
    if (parity x = p) then (p, i) else (Top, i)

let universe = (P.Top, I.Top)

let singleton v = P.singleton v, I.singleton v

let of_bounds x = P.of_bounds x, I.of_bounds x

let contains (p1,i1) (p2,i2) = P.contains p1 p2 && I.contains i1 i2

let join (p1,i1) (p2,i2) = reduce (P.join p1 p2) (I.join i1 i2)

let widen (p1,i1) (p2,i2) = reduce (P.widen p1 p2) (I.widen i1 i2)

let neg (p1,i1) = (P.neg p1), (I.neg i1)

let is_safe_add (p1,i1) (p2,i2) = P.is_safe_add p1 p2 && I.is_safe_add i1 i2

let is_safe_mult (p1,i1) (p2,i2) = P.is_safe_mult p1 p2 && I.is_safe_mult i1 i2

let is_safe_div (p1,i1) (p2,i2) = P.is_safe_div p1 p2 && I.is_safe_div i1 i2

let is_safe_minus (p1,i1) (p2,i2) = P.is_safe_minus p1 p2 && I.is_safe_minus i1 i2

let minus (p1, i1) (p2, i2) = reduce (P.minus p1 p2) (I.minus i1 i2)

let mult (p1, i1) (p2, i2) = reduce (P.mult p1 p2) (I.mult i1 i2)

let div (p1, i1) (p2, i2) = reduce (P.div p1 p2) (I.div i1 i2)

let add (p1, i1) (p2, i2) = reduce (P.add p1 p2) (I.add i1 i2)

let implies ((p1, i1), cmp, simpl) = (P.implies (p1, cmp, simpl)) && (I.implies (i1, cmp, simpl))

(* Restricts the value x to make the condition
   c op x true *)
let guard op (p1, i1) (p2, i2) = reduce (P.guard op p1 p2) (I.guard op i1 i2)

let to_string (p1, i1) = Printf.sprintf "(%s,%s)" (P.to_string p1) (I.to_string i1)
