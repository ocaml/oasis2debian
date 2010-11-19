
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
           List.exists (fun fn -> String.ends_with fn ".so") generated_files
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
                          chn)
                     roots);
              
              dh_with_fn deb_runtime "install.in"
                (fun chn ->
                   (* At least one findlib root has a dll *)
                   if List.exists snd roots then
                     output_content "@OCamlDllDir@/*.so" chn;

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
            mk_ocamldoc deb_pkg;

            (* TODO *)
            ()

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
