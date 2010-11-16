
open OASISTypes
open FileUtil
open Common

let dh_compat = "7"

let description = 
  Conf.create 
    ~cli:"--description"
    "Long description of the package"
    Conf.LongInput

let homepage =
  Conf.create 
    ~cli:"--homepage"
    "Homepage of the package"
    Conf.ShortInput

let uploader =
  Conf.create 
    ~cli:"--uploader"
    "Uploader of the package"
    (Conf.Fun
       (fun () ->
          try 
            Printf.sprintf 
              "%s <%s>"
              (Sys.getenv "DEBFULLNAME")
              (Sys.getenv "DEBEMAIL")
          with _ ->
            failwith "Unable to guess uploader"))

let () = 
  let ctxt = 
    {(!OASISContext.default) with 
         OASISContext.ignore_plugins = true}
  in

  let pkg = 
    OASISParse.from_file
      ~ctxt
      "_oasis"
  in

  let dflt r x =
    match x, Conf.is_set r with 
      | Some e, false -> 
          Conf.set r e
      | _ ->
          ()
  in

  let () = 
    dflt description pkg.OASISTypes.description;
    dflt homepage    pkg.OASISTypes.homepage
  in

  let () = 
    Arg.parse
      (Arg.align !Conf.all_args)
      (fun s -> 
         failwith 
           (Printf.sprintf
              "Don't know what to do with '%s'"
              s))
      (Printf.sprintf 
         "oasis2debian v%s by Sylvain Le Gall"
         Version.ver)
  in

  let t = 
    {
      build_depends = [];
      description   = Conf.get description;
      homepage      = Conf.get homepage;
      uploader      = Conf.get uploader;
      pkg           = pkg;
      deb_std       = None;
      deb_dev       = None;
      deb_doc       = None;
    }
  in

  let lib, doc, bin =
    List.fold_left
      (fun ((lib, doc, bin) as acc) ->
         function
           | Library (cs, bs, lib') ->
               ((cs, bs, lib') :: lib), doc, bin
           | Executable (cs, bs, exec) ->
               lib, doc, ((cs, bs, exec) :: bin)
           | Doc (cs, doc') ->
               lib, ((cs, doc') :: doc), bin
           | Flag _ | Test _ | SrcRepo _ ->
               acc)
      ([], [], [])
      pkg.sections
  in

  let arch lst = 
    let is_all = 
      List.for_all
        (function
           | {bs_install = [OASISExpr.EBool true, false]} ->
               (* No install, don't take into account for arch *)
               true 
                 
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
    (* Try to guess the target name *)
    List.fold_left
      (fun name pat -> 
         Pcre.replace ~pat ~templ:"" name)

      (* Start with the package name *)
      t.pkg.OASISTypes.name
      ["^ocaml-?"; "-?ocaml$"]
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

  (* Create debian/ and debian/compat *)
  let () = 
    debian_with_fn "compat"
      (fun chn -> output_string chn (dh_compat^"\n"))
  in

  (* Create debian/source *)
  let () = 
    debian_with_fn "source/format"
      (output_content "3.0 (quilt)")
  in

  (* Create debian/changelog *)
  let () = 
    let pkg_version = 
      OASISVersion.string_of_version t.pkg.version
    in

    if debian_not_exist "changelog" then
      assert_command 
        (interpolate 
           "dch --create --package $pkg.OASISTypes.name --newversion $pkg_version-1")
  in

  let () = 
    Rules.create t;
    Control.create t;
    Copyright.create t;
  in 

    ()
