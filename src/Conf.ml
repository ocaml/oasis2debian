(******************************************************************************)
(* oasis2debian: Create and maintain Debian package for an OASIS package      *)
(*                                                                            *)
(* Copyright (C) 2013, Sylvain Le Gall                                        *)
(*                                                                            *)
(* This library is free software; you can redistribute it and/or modify it    *)
(* under the terms of the GNU Lesser General Public License as published by   *)
(* the Free Software Foundation; either version 2.1 of the License, or (at    *)
(* your option) any later version, with the OCaml static compilation          *)
(* exception.                                                                 *)
(*                                                                            *)
(* This library is distributed in the hope that it will be useful, but        *)
(* WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY *)
(* or FITNESS FOR A PARTICULAR PURPOSE. See the file COPYING for more         *)
(* details.                                                                   *)
(*                                                                            *)
(* You should have received a copy of the GNU Lesser General Public License   *)
(* along with this library; if not, write to the Free Software Foundation,    *)
(* Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA              *)
(******************************************************************************)

type 'a t =
  | Value of 'a
  | LongInput
  | ShortInput
  | Fun of (unit -> 'a)

type 'a rcrd =
    {
      mutable value: 'a t;
      parse: string -> 'a;
      help: string;
    }

let all_args : ((string * Arg.spec * string) list) ref =
  ref []

let set r x =
  r.value <- Value x

let is_set r =
  match r.value with
    | Value _ -> true
    | _ -> false

let rec get ~ctxt r =
  match r.value with
    | Fun f ->
        set r (f ());
        get ~ctxt r

    | LongInput ->
        set r (r.parse (Input.long ~ctxt "" r.help));
        get ~ctxt r

    | ShortInput ->
        set r (r.parse (Input.short ~ctxt (r.help^": ")));
        get ~ctxt r

    | Value x ->
        x

let create_full ?cli parse help t =
  let res =
    {
      value = t;
      parse = parse;
      help  = help;
    }
  in
    begin
      match cli with
        | Some cli ->
            all_args :=
            (cli,
             Arg.String (fun s -> set res (parse s)),
             help)
            :: !all_args
        | None -> ()
    end;
    res

let create ?cli help t =
  create_full ?cli (fun s -> s) help t
