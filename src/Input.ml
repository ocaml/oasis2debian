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

open ExtString
open Common
open FileUtil

(** Edit a text in an editor *)
let long ~ctxt text help =
  let fn, chn =
    Filename.open_temp_file "oasis2debian-" ".txt"
  in

  let read_content () =
    let chn =
      open_in fn
    in
    let buf = Bytes.make (in_channel_length chn) 'X' in
    let () =
      really_input chn buf 0 (Bytes.length buf);
      close_in chn
    in
    let lst =
      List.filter
        (fun s -> not (String.starts_with s "#"))
        (String.nsplit (Bytes.to_string buf) "\n")
    in

    let rec chop_blank_eol str =
      if String.ends_with str " " ||
         String.ends_with str "\t" ||
         String.ends_with str "\r" then
        chop_blank_eol (String.rchop str)
      else
        str
    in

    let lst =
      List.rev_map chop_blank_eol lst
    in

    let rec chop_blank_last_lines =
      function
        | "" :: tl ->
            chop_blank_last_lines tl
        | lst ->
            List.rev lst
    in

    let lst =
      chop_blank_last_lines lst
    in

      String.concat "\n" lst
  in


    try
      begin
        let res =
          (* Write the initial content *)
          output_string chn text;
          output_string chn
            (String.concat "\n# "
               ("" ::
                "Type your text above, lines starting with \
                 '#' will be ignored" :: (String.nsplit help "\n")));
          close_out chn;

          (* Run the editor (debian specific) *)
          assert_command ~ctxt (Printf.sprintf "sensible-editor %s" fn);

          (* Get back the text *)
          read_content ()
        in
          rm [fn];
          res
      end

    with e ->
      rm [fn];
      raise e

let short ~ctxt:_ q =
  print_string q; flush stdout;
  read_line ()
