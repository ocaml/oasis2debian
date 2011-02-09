
(** Init action 
  *)

open OASISTypes
open OASISMessage
open FileUtil
open Common

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

let dh_compat = "7"

let run ~ctxt args = 

  let t = 
    Load.load ~ctxt args
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
