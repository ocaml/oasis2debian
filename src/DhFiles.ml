
(** Create debhelper files for generated packages
  *)

open OASISTypes
open OASISLibrary 
open ExtString
open Common

let create ~ctxt t = 
  let dh_with_fn deb_pkg ext = 
    debian_with_fn (deb_pkg.name^"."^ext)
  in

  let findlib_roots = 
    OASISLibrary.group_libs t.pkg_generic 
  in

  let roots = 
    List.rev_map 
      (fun grp ->
         let findlib_name = 
           OASISLibrary.findlib_of_group grp 
         in

         let libs = 
           let rec fold acc =
             function
               | Container (_, lst) ->
                   List.fold_left fold acc lst
               | Package (_, cs, bs, lib, lst) ->
                   List.fold_left fold ((cs, bs, lib) :: acc) lst
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
                  (fun acc e -> 
                     let fns = 
                       OASISLibrary.generated_unix_files
                         ~ctxt
                         e
                         Sys.file_exists
                         (fun () -> true)
                         (fun () -> ".a")
                         (fun () -> ".so")
                     in
                       fns :: acc) 
                  []
                  libs))
         in
           
         (* Has_dll *)
         let has_dll = 
           List.exists 
             (fun fn -> String.ends_with fn ".so") 
             generated_files
         in

           findlib_name, has_dll)

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

         | Flag _ | Library _ | Executable _ | SrcRepo _ | Test _ ->
             false)
      t.pkg_generic.sections
  in

  let mk_ocamldoc deb_pkg =
    if not has_apidoc then
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

             | Library _ | Executable _ | Flag _ 
             | Test _ | SrcRepo _ ->
                 n)
        1
        t.pkg_generic.sections
    in
      ()
  in

    begin 
      match t.deb_dev with 
        | Some (deb_dev, deb_runtime) ->
            begin
              dh_with_fn deb_dev "install.in"
                (fun chn -> 
                   List.iter 
                     (fun (findlib_name, has_dll) ->
                        
                        output_content 
                          (interpolate 
                             "\
@OCamlStdlibDir@/$findlib_name/*.cm[ix]
@OCamlStdlibDir@/$findlib_name/*.ml*
OPT: @OCamlStdlibDir@/$findlib_name/*.cmxa")
                          chn;

                        output_content
                          (if has_dll then
                             (interpolate 
                                "@OCamlStdlibDir@/$findlib_name/*.a")
                           else
                             (interpolate 
                                "OPT: @OCamlStdlibDir@/$findlib_name/*.a"))
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
                        | (findlib_name, true) ->
                            output_content 
                              ("@OCamlStdlibDir@/"^findlib_name^"/*.so @OCamlDllDir@")
                              chn
                        | (_, false) ->
                            ())
                     roots;

                   List.iter 
                     (fun (findlib_name, has_dll) ->
                        output_content 
                          (interpolate 
                             "\
@OCamlStdlibDir@/$findlib_name/META
@OCamlStdlibDir@/$findlib_name/*.cm[ao]")
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
