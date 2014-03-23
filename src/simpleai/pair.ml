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

type t =
  | Top
  | Even
  | Odd

let universe = Top

let singleton i =
  let m = Int32.rem i (Int32.of_int 2) in
  begin
    match m with
    | zero -> Even
    | one -> Odd
  end

let of_bounds (l, u) =  if Int32.compare l u = 0 then (singleton l) else Top

let contains x y =
  match (x, y) with
      (Top, _) -> true
    | (Even, Even) -> true
    | (Odd, Odd) -> true
    | _ -> false

let join x y =
  match (x, y) with
    | (x, y) when x = y -> x
    | _ -> Top

let widen = join

let neg x = Top

let is_safe_add x y = true

let is_safe_mult x y = false

let is_safe_div x y = false

let is_safe_minus x y = true

let minus x y =
  match (x, y) with
    | (x, y) when x == y -> Even
    | (Even, Odd) -> Odd
    | (Odd, Even) -> Odd
    | _ -> Top

let mult x y =
  match (x, y) with
    | (Odd, Odd) -> Odd
    | (Even, Odd) -> Even
    | (Odd, Even) -> Even
    | (Even, Even) -> Even
    | _ -> Top

let div x y =
  match (x, y) with
    | _ -> Top

let add = minus

let implies (x, cmp, simpl) = match (x, cmp) with
  | (Top, _) -> false
  | (x, Simple.Equals) -> x = (singleton simpl)
  | (_, Simple.IsLess) -> false

(* Restricts the value x to make the condition
   c op x true *)
let guard op c x =
  match (op, c, x) with
    | (LT, Even, Odd) -> raise Emptyset
    | (GT, Even, Odd) -> raise Emptyset
    | (EQ, x, y) when x <> y -> raise Emptyset
    | _ -> x

let to_string v =
  match v with
    | Even -> "Even"
    | Odd -> "Odd"
    | Top -> "?"
