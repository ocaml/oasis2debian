
(** Update action 
  *)

open OASISMessage
open OASISTypes
open DebianFormats
open Common

module S = BuildDepends.SetDepends

let diff_depends ~ctxt lst1 lst2 = 
  let to_set lst = 
    List.fold_left (fun st e -> S.add e st) S.empty lst
  in
  (* Compute dependency added/removed *)
  let st1 = to_set lst1 in
  let st2 = to_set lst2 in
  let adds = S.diff st2 st1 in
  let dels = S.diff st1 st2 in

    (* TODO: compute dependency upgraded/downgraded *)
    S.iter 
      (fun b ->
         warning ~ctxt
           "New dependency: %s"
           (BuildDepends.to_string b))
      adds;
    S.iter
      (fun b ->
         warning ~ctxt
           "Dependency removed: %s"
           (BuildDepends.to_string b))
      dels

let run ~ctxt args = 

  let t = 
    Load.load ~ctxt args
  in

  (* TODO: move this to debian-formats *)
  let with_fn fn f = 
    let chn = 
      open_in fn
    in
      try 
        let res =
          f (IO.input_channel chn)
        in
          close_in chn;
          res
      with e ->
        close_in chn;
        raise e
  in

  let ctl_source, ctl_binaries = 
    with_fn "debian/control" Control.parse
  in

  let changelog = 
    with_fn "debian/changelog" Changelog.head
  in

  let ctl_build_depends =
    List.map 
      (function
         | ((pkg, None), _) :: _ -> 
             pkg, None, `All
         | ((pkg, Some (op, ver)), _) :: _ -> 
             pkg, 
             Some (OASISVersion.comparator_of_string (op^" "^ver)), 
             `All
         | [] -> 
             invalid_arg "ctl_build_depends")
      ctl_source.Control.build_depends
  in

  let () =
    diff_depends ~ctxt ctl_build_depends t.build_depends
  in

  let () = 
    let oasis_version =  
      OASISVersion.string_of_version t.pkg_generic.version
    in
      (* TODO: take into account EPOCH et al *)
      if oasis_version <> changelog.Changelog.version then
        warning ~ctxt
          "New version '%s', run 'dch -v %s-1 \"New upstream release\"'"
          oasis_version oasis_version
  in

    ()

