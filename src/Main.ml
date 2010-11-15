
open OASISTypes
open FileUtil
open Common

let dh_compat = "7"

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

  let dflt x d =
    match x with 
      | Some e -> e
      | None -> d
  in

  let t = 
    {
      build_depends = [];
      description   = dflt pkg.OASISTypes.description "TODO";
      homepage      = dflt pkg.OASISTypes.homepage "TODO";
      uploader      = Printf.sprintf 
                        "%s <%s>"
                        (Sys.getenv "DEBFULLNAME")
                        (Sys.getenv "DEBEMAIL");
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

  (* Create debian/rules *)
  let () = 
    debian_with_fn "rules"
      (output_content "\
#!/usr/bin/make -f
# -*- makefile -*-

# Uncomment this to turn on verbose mode.
#export DH_VERBOSE=1

DESTDIR=$(CURDIR)/debian/ocamlify

include /usr/share/ocaml/ocamlvars.mk

%:
	dh --with ocaml $@

.PHONY: override_dh_auto_configure
override_dh_auto_configure:
	ocaml setup.ml -configure --prefix /usr --destdir '$(DESTDIR)' --enable-debug

.PHONY: override_dh_auto_build
override_dh_auto_build:
	ocaml setup.ml -build

.PHONY: override_dh_auto_install
override_dh_auto_install:
	mkdir -p '$(DESTDIR)/usr/bin'
	ocaml setup.ml -install 

.PHONY: override_dh_auto_clean
override_dh_auto_clean:
	ocaml setup.ml -distclean")
  in
  let () = 
    Unix.chmod (debian_fn "rules") 0o655 
  in

  let () = 
    Control.create t;
    Copyright.create t;
  in 

    ()

(*
 
oasis2debian -itp ?
 
create copyright file
-> Copyrights mandatory
-> ajouter le texte de l'exception OCaml

create control file

 *)
