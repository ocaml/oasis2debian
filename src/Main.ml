
open OASISTypes
open OASISMessage
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

let deb_name =
  Conf.create 
    ~cli:"--debian-name"
    "Source package name in Debian (e.g. extunix become ocaml-extunix)"
    Conf.ShortInput

let itp =
  Conf.create
    ~cli:"--itp"
    "Bug number of the ITP for the package"
    Conf.ShortInput

let bts_query =
  Conf.create_full 
    ~cli:"--bts-query"
    bool_of_string
    "Query the BTS for ITP (true/false)"
    (Conf.Value true)

let () = 
  let () = 
    (* Clean ENV *)
    Unix.putenv "OCAMLPATH" "";
    Unix.putenv "LC_ALL" "C"
  in

  let ctxt = 
    {(!OASISContext.default) with 
         OASISContext.ignore_plugins = true}
  in

  let pkg = 
    OASISParse.from_file
      ~ctxt
      "_oasis"
  in
                 
  let expr = 
    Expr.create ~ctxt pkg
  in

  let pkg_generic =
    PkgGeneric.create ~ctxt expr pkg
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
    let cur_dn = FilePath.dirname (pwd ()) in
    let pkg_nm = pkg.OASISTypes.name in
      if pkg_nm = cur_dn then
        Conf.set deb_name cur_dn
      else
        warning ~ctxt 
          "OASIS name (%s) and directory name (%s) are not the same, \
           cannot set Debian name"
          pkg_nm cur_dn
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
      build_depends = BuildDepends.get ~ctxt pkg [];
      description   = Conf.get ~ctxt description;
      homepage      = Conf.get ~ctxt homepage;
      uploader      = Conf.get ~ctxt uploader;
      deb_name      = Conf.get ~ctxt deb_name;
      pkg           = pkg;
      pkg_generic   = pkg_generic;
      expr          = expr;
      deb_std       = None;
      deb_dev       = None;
      deb_doc       = None;
    }
  in

  let t = 
    (* Fix package generated *)
    GenPkg.set ~ctxt t
  in

  (* Create debian/ and debian/compat *)
  let () = 
    debian_with_fn "compat"
      (fun chn -> output_string chn (dh_compat^"\n"))
  in

  (* Create debian/gbp.conf *)
  let () = 
    debian_with_fn "gbp.conf"
      (fun chn ->
         output_string chn
           "[DEFAULT]\n\
            pristine-tar = True\n\
            cleaner = debuild clean && dh_quilt_unpatch && dh_clean\n")
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
      begin
        let itp = 
          Conf.get ~ctxt itp 
        in
        let opts =
          ""
        in
        let opts =
          if Conf.get ~ctxt bts_query then
            opts
          else
            opts^" --no-query"
        in
          assert_command ~ctxt  
            (interpolate 
               "dch --create --package $t.deb_name --newversion $pkg_version-1 --closes $itp $opts")
      end
  in

  let () = 
    Control.create t;
    Copyright.create ~ctxt t;
    Rules.create t;
    DhFiles.create ~ctxt t
  in 

    ()
