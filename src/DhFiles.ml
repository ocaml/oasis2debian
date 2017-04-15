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

(** Create debhelper files for generated packages
  *)

open OASISTypes
open OASISFindlib
open ExtString
open BuildDepends
open Common

type t =
  {
    findlib_name: string;
    has_byte:     bool;
    has_native:   bool;
    has_dll:      bool;
    has_cmi:      bool;
    has_cmxs:     bool;
  }

let create ~ctxt t =
  let dh_with_fn deb_pkg ext =
    debian_with_fn (deb_pkg.name^"."^ext)
  in

  let findlib_roots, _, _ =
    OASISFindlib.findlib_mapping t.pkg_generic
  in

  let roots =
    List.rev_map
      (fun grp ->
         let findlib_name =
           OASISFindlib.findlib_of_group grp
         in

         let libs =
           let rec fold acc =
             function
               | Container (_, lst) ->
                   List.fold_left fold acc lst
               | Package (_, cs, bs, a, _, lst) ->
                   List.fold_left fold ((cs, bs, a) :: acc) lst
           in
             fold [] grp
         in

         (* We compute the set of possible generated files to test
          * them for certain properties.
          *)
         let generated_files =
           List.flatten
             (List.flatten
                (List.fold_left
                  (fun acc ->
                     function
                       | cs, bs, `Library lib ->
                           let fns =
                             OASISLibrary.generated_unix_files
                               ~ctxt
                               ~source_file_exists:Sys.file_exists
                               ~is_native:true
                               ~has_native_dynlink:
                               (bool_of_string
                                  (BaseStandardVar.native_dynlink ()))
                               ~ext_lib:".a"
                               ~ext_dll:".so"
                               (cs, bs, lib)
                           in
                             fns :: acc
                       | cs, bs, `Object obj ->
                           let fns =
                             OASISObject.generated_unix_files
                               ~ctxt
                               ~source_file_exists:Sys.file_exists
                               ~is_native:true
                               (cs, bs, obj)
                           in
                             fns :: acc)
                  []
                  libs))
         in

         let has_extensions exts =
           List.fold_left
             (fun acc ext ->
                List.exists
                  (fun fn -> String.ends_with fn ext)
                  generated_files
                || acc)
             false
             exts
         in

           {
             findlib_name = findlib_name;
             has_dll      = has_extensions [".so"];
             has_native   = has_extensions [".cmxa"; ".cmx"];
             has_byte     = has_extensions [".cma"; ".cmo"];
             has_cmi      = has_extensions [".cmi"];
             has_cmxs     = has_extensions [".cmxs"];
           })

      findlib_roots
  in

  let has_apidoc =
    List.exists
      (function
         | Doc (cs, doc) ->
             (* We estimate that a doc is an API reference if it uses
              * ocamldoc.
              *)
             List.mem
               (ExternalTool "ocamldoc")
               doc.doc_build_tools

         | Flag _ | Object _ | Library _ | Executable _ | SrcRepo _ | Test _ ->
             false)
      t.pkg_generic.sections
  in

  let mk_ocamldoc deb_pkg =
    if not has_apidoc &&
       (* API doc is not already generated *)

       List.exists (fun e -> e.has_cmi) roots then
       (* There are .mli/.ml to create API doc *)

      begin
        dh_with_fn deb_pkg "ocamldoc"
          (if t.findlib_packages = [] then
             output_content "# Nothing"
           else
             output_content (" -package " ^
                             (String.concat "," t.findlib_packages)))
      end
  in

  let mk_doc_base docdir cs doc chn =
    let print s = output_content s chn in
    let installdir = var_expand t doc.doc_install_dir in
      print
        (interpolate "\
Document: $t.deb_name-$cs.cs_name
Title: $doc.doc_title
Section: Programming/OCaml");
      begin
        match doc.doc_abstract with
          | Some str ->
              print ("Abstract: "^str)
          | None ->
              ()
      end;

      begin
        match doc.doc_authors with
          | [] ->
              ()
          | lst ->
              print
                ("Author: "^(String.concat ", " lst))
      end;

      print "";

      begin
        match doc.doc_format with
          | HTML index ->
              print "Format: HTML";
              print ("Index: "^installdir^"/"^index)

          | DocText ->
              print "Format: Text"

          | PDF ->
              print "Format: PDF"
          | PostScript ->
              print "Format: PostScript"

          | Info index ->
              print "Format: Info";
              print ("Index: "^installdir^"/"^index)

          | DVI ->
              print "Format: DVI"

          | OtherDoc ->
              (* Default to HTML with a fake index *)
              print "Format: HTML";
              print ("Index: "^installdir^"/index.html")
      end;

      print ("Files: "^installdir^"/*")
  in

  let mk_doc_bases deb_pkg docdir =
    let _i : int =
      List.fold_left
        (fun n ->
           function
             | Doc (cs, doc) ->
                 if not (DhManpages.doc_is_manpage cs doc) then begin
                   dh_with_fn deb_pkg
                     ("doc-base."^(string_of_int n))
                     (mk_doc_base docdir cs doc);
                   n + 1
                 end else begin
                   n
                 end

             | Object _ | Library _ | Executable _
             | Flag _ | Test _ | SrcRepo _ ->
                 n)
        1
        t.pkg_generic.sections
    in
      ()
  in

    begin
      match t.deb_exec, t.deb_dev with
        | Some deb_pkg, Some _ ->
            dh_with_fn deb_pkg "install"
              (output_content "usr/bin");
        | Some _, None
        | None, _ ->
            ()
    end;

    begin
      match t.deb_dev with
        | Some (deb_dev, deb_runtime) ->
            begin
              dh_with_fn deb_dev "install.in"
                (fun chn ->
                   List.iter
                     (fun e ->

                        if e.has_cmi then
                          output_content
                            (interpolate
                               "@OCamlStdlibDir@/$e.findlib_name/*.cmi")
                            chn;

                        if e.has_cmi then
                          output_content
                            (interpolate
                               "@OCamlStdlibDir@/$e.findlib_name/*.ml*")
                          chn;

                        if e.has_native then
                          begin
                            output_content
                              (interpolate
                                 "OPT: @OCamlStdlibDir@/$e.findlib_name/*.cmx")
                              chn;
                            output_content
                              (interpolate
                                 "OPT: @OCamlStdlibDir@/$e.findlib_name/*.cmxa")
                              chn
                          end;

                        if e.has_dll then
                          output_content
                            (interpolate
                               "@OCamlStdlibDir@/$e.findlib_name/*.a")
                            chn
                        else if e.has_native then
                          output_content
                            (interpolate
                               "OPT: @OCamlStdlibDir@/$e.findlib_name/*.a")
                            chn;

                        begin
                          match t.deb_doc, docdir t with
                            | None, Some fn ->
                                output_content
                                  (FilePath.make_relative "/" fn)
                                  chn;

                                mk_doc_bases deb_dev fn
                            | _, _ ->
                                ()
                        end;

                     )
                     roots);

              dh_with_fn deb_runtime "install.in"
                (fun chn ->
                   (* At least one findlib root has a dll *)
                   List.iter
                     (function
                        | {has_dll = true} as e ->
                            output_content
                              ("@OCamlStdlibDir@/"^
                               e.findlib_name^"/*.so @OCamlDllDir@")
                              chn
                        | _ ->
                            ())
                     roots;

                   List.iter
                     (fun e ->
                        output_content
                          (interpolate
                             "@OCamlStdlibDir@/$e.findlib_name/META")
                          chn;
                        if e.has_byte then
                          output_content
                            (interpolate
                               "@OCamlStdlibDir@/$e.findlib_name/*.cm[ao]")
                            chn;
                        if e.has_cmxs then
                          output_content
                            (interpolate
                               "OPT: @OCamlStdlibDir@/$e.findlib_name/*.cmxs")
                            chn)
                     roots)
            end

        | None ->
            ()
    end;

    begin
      match t.deb_doc with
        | Some deb_pkg ->
            begin
              mk_ocamldoc deb_pkg;

              match docdir t with
                | Some docdir ->
                    begin
                      dh_with_fn deb_pkg "install"
                        (output_content
                           (FilePath.make_relative "/" docdir));

                      mk_doc_bases deb_pkg docdir
                    end

                | None ->
                    ()
            end

        | None ->
            begin
              (* We need to attach the ocamldoc to some package *)
              match t.deb_dev with
                | Some (deb_pkg, _) ->
                    mk_ocamldoc deb_pkg
                | None ->
                    ()
            end
    end

let insertion_point =
  "# Insertion point for oasis2debian, do not remove."

let snippet_start snippet_name =
  Printf.sprintf "# oasis2debian snippet '%s' start." snippet_name

let snippet_end snippet_name =
  Printf.sprintf "# oasis2debian snippet '%s' end." snippet_name


(** Split a script file around its insertion point.
  *)
let dh_script_split fn =
  let rec split =
    function
      | hd :: tl ->
          if hd = insertion_point then
            [], hd, tl
          else
            let before, cur, after =
              split tl
            in
              hd :: before, cur, after
      | [] ->
          failwith
            (Printf.sprintf
               "'%s' not found in file '%s'"
               insertion_point fn)
  in
    if debian_not_exist fn then
      debian_with_fn
        fn
        (output_content
           (interpolate "\
#!/bin/sh

set -e

#DEBHELPER#

$insertion_point"));
    split (lines_of_file (debian_fn fn))


(** Append data to debian/pkg.postinst.
  *)
let dh_postinst pkg snippet_name content =
  let basename = pkg^".postinst" in
  let before, cur, after =
    dh_script_split basename
  in
    file_of_lines
      (debian_fn basename)
      (before @
       [snippet_start snippet_name;
        content;
        snippet_end snippet_name;
        ""; cur] @ after)

(** Prepend data to debian/pkg.prerm.
  *)
let dh_prerm pkg snippet_name content =
  let basename = pkg^".prerm" in
  let before, cur, after =
    dh_script_split basename
  in
    file_of_lines
      (debian_fn basename)
      (before @
       [cur; ""; snippet_start snippet_name;
        content;
        snippet_end snippet_name] @ after)
