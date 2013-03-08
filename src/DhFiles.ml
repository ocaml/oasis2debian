(******************************************************************************)
(* oasis2debian: create and maintain a debian/ directory using _oasis         *)
(*                                                                            *)
(* Copyright (C) 2010, OCamlCore SARL, http://www.ocamlcore.com               *)
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
open Common

type t = 
  {
    findlib_name: string;
    has_byte:     bool;
    has_native:   bool;
    has_dll:      bool;
    has_cmi:      bool;
    (* TODO: dynlink *)
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
               | Package (_, cs, bs, a, lst) ->
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
                               ~has_native_dynlink:true
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
           })

      findlib_roots
  in

  let has_apidoc = 
    List.exists
      (function
         | Doc (cs, doc) ->
             (* We estimate that a doc is an API reference * if it uses
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
          (output_content "# Nothing")
      end
  in

  let mk_doc_base docdir cs doc chn = 
    let print s = output_content s chn in
    let installdir = 
      (* TODO: something better *)
      (* Close your eyes *)
      Unix.putenv "prefix" "/usr";
      Unix.putenv "docdir" docdir;
      BaseStandardVar.init t.pkg_generic;
      BaseEnv.var_expand doc.doc_install_dir
      (* You can open your eyes again *)
    in
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
                 dh_with_fn deb_pkg 
                   ("doc-base."^(string_of_int n))
                   (mk_doc_base docdir cs doc);
                 n + 1

             | Object _ | Library _ | Executable _ 
             | Flag _ | Test _ | SrcRepo _ ->
                 n)
        1
        t.pkg_generic.sections
    in
      ()
  in

    begin
      match t.deb_exec with 
        | Some deb_pkg ->
            dh_with_fn deb_pkg "install"
              (output_content "usr/bin")
        | None ->
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
                          output_content
                            (interpolate
                               "OPT: @OCamlStdlibDir@/$e.findlib_name/*.cmx*")
                            chn;

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
                              ("@OCamlStdlibDir@/"^e.findlib_name^"/*.so @OCamlDllDir@")
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
