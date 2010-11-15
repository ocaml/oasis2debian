
open OUnit
open FileUtil
open FilePath

let oasis2debian =
  ref "_build/src/oasis2debian"

let tests = 
  let dirs = 
    ls (make_filename ["test"; "data"])
  in
  let pwd =
    pwd ()
  in
    List.map 
      (fun dn ->
         dn >::
         bracket 
           (fun () ->
              Sys.chdir dn)
           (fun () ->
              Sys.command !oasis2debian)
           (fun () ->
              rm ~recurse:true ["debian"];
              Sys.chdir pwd))
      dirs
      

let _ =
  Unix.putenv "EDITOR" "true";
  run_test_tt_main
    ~arg_specs:["-exec",
                Arg.Set_string oasis2debian,
                "prg oasis2debian program to test"]
    ("oasis2debian" >::: tests)
