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

let speclist =
  [
    ("--verbose", Arg.Unit Context.set_verbose, "prints more details");
    ("--unroll", (Arg.Int Context.set_unroll), "unrolls the loops n-times");
    ("--delay", (Arg.Int Context.set_delay), "delays the widening n-times");
    ("--to-dot", Arg.String (fun s -> Context.dot_filename := s;Context.dot_output := true),  "outputs a DOT representation of the CFG")
  ]

let process input =
  let prog = Newspeak.read input in
  let simple = Filter.process prog in
  if (!Context.dot_output) then
    Simple.to_dot simple !Context.dot_filename
  else
    print_string (Printf.sprintf "Unrolling:%d\n" !Context.unroll);
    Solver.compute simple !Context.unroll !Context.delay

let _ =
  StandardApplication.launch_process_with_npk_argument "simpleai" speclist
    process
