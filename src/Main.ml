
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
      build_depends = BuildDepends.get ~ctxt pkg [];
      description   = Conf.get ~ctxt description;
      homepage      = Conf.get ~ctxt homepage;
      uploader      = Conf.get ~ctxt uploader;
      pkg           = pkg;
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
      assert_command ~ctxt  
        (interpolate 
           "dch --create --package $pkg.OASISTypes.name --newversion $pkg_version-1")
  in

  let () = 
    Rules.create t;
    Control.create t;
    Copyright.create t;
  in 

    ()
