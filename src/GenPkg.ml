
(** Compute package to generate 
  *)

open OASISTypes
open Common

let library_name = 
  Conf.create 
    ~cli:"--library-name"
    "Short name of the library (XXX in libXXX-ocaml-dev)"
    (Conf.Fun
       (fun () ->
          failwith "Not set"))

let set ~ctxt t = 
  let eval = 
    Expr.choose 
      ~ctxt 
      t.expr 
      (`All (fun x y -> x || y))
  in

  let lib, doc, bin =
    List.fold_left
      (fun ((lib, doc, bin) as acc) ->
         function
           | Library (cs, bs, lib') ->
               if eval bs.bs_build && eval bs.bs_install then
                 ((cs, bs, lib') :: lib), doc, bin
               else
                 acc

           | Executable (cs, bs, exec) ->
               if eval bs.bs_build && eval bs.bs_install then
                 lib, doc, ((cs, bs, exec) :: bin)
               else
                 acc

           | Doc (cs, doc') ->
               if eval doc'.doc_build && eval doc'.doc_install then
                 lib, ((cs, doc') :: doc), bin
               else
                 acc

           | Flag _ | Test _ | SrcRepo _ ->
               acc)
      ([], [], [])
      t.pkg.sections
  in

  let arch lst = 
    let is_all = 
      List.for_all
        (function
           |  {bs_compiled_object = Byte} ->
               true

           | _ -> 
               false)
        (List.rev_map 
           (fun (_, bs, _) -> bs)
           lst)
    in
      if is_all then
        "all"
      else
        "any"
  in

  let mk_deb nm lst = 
    {name = nm; arch = arch lst}
  in

  let base_name = 
    if Conf.is_set library_name then
      begin
        Conf.get ~ctxt library_name 
      end
    else
      begin
        match OASISLibrary.group_libs t.pkg with 
          | [hd] ->
              (* First method: if there is a single findlib library use its name
               *)
              OASISLibrary.findlib_of_group hd

          | _ ->
              (* Default method: try to guess the target name using source name 
               *)
              List.fold_left
                (fun name pat -> 
                   Pcre.replace ~pat ~templ:"" name)

                (* Start with the package name *)
                t.pkg.OASISTypes.name
                ["^ocaml-?"; "-?ocaml$"]
      end
  in

  let spf fmt = 
    Printf.sprintf fmt
  in

  let add_doc nm t = 
    (* Add doc package, only if more than one documentation
     * shipped.
     *)
    if List.length doc > 1 then
      {t with 
           deb_doc = Some (mk_deb nm [])}
    else
      t
  in

  let t =
    (* Determine if we have bin+dev+runtime *)
    match lib, bin with
      | [], bin ->
          begin
            (* Only a binary package, name = source name *)
            let base_name = 
              t.pkg.OASISTypes.name
            in
              add_doc 
                (base_name^"-doc")
                {t with deb_std = Some (mk_deb base_name bin)} 
          end

      | lib, bin ->
          begin 
            (* Library only *)
            let t  = 
              {t with 
                   deb_dev = 
                     Some (mk_deb (spf "lib%s-ocaml-dev" base_name) lib,
                           mk_deb (spf "lib%s-ocaml" base_name) lib)}
            in

            (* Also executables ? *)
            let t =
              if bin <> [] then
                {t with 
                     deb_std = 
                       Some (mk_deb (spf "lib%s-ocaml-bin" base_name) bin)}
              else
                t
            in

              add_doc 
                (spf "lib%s-ocaml-doc" base_name)
                t
          end
  in

    t
