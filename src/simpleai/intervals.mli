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

type t = Top | Bottom | Val of int32 * int32

val universe: t
val singleton: Int32.t -> t
val of_bounds: (Int32.t * Int32.t) -> t
val join: t -> t -> t
val widen: t -> t -> t
val contains: t -> t -> bool
val implies: (t * Simple.cmp * Int32.t) -> bool
val neg: t -> t
val minus: t -> t -> t
val mult: t -> t -> t
val div: t -> t -> t
val add: t -> t -> t
val is_safe_add: t -> t -> bool
val is_safe_minus: t -> t -> bool
val is_safe_mult: t -> t -> bool
val is_safe_div: t -> t -> bool
val guard: bop -> t -> t -> t
val to_string: t -> string
