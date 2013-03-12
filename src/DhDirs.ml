
open Common

let dirs = 
  let lst = ref [] in
    Conf.create_full
      ~cli:"--dh-dirs"
      (fun s ->
         Scanf.sscanf 
           s "%s@,%s"
           (fun pkg fn ->
              let lst' = 
                (pkg, fn) :: !lst
              in
                lst := lst';
                lst'))
      "pkg,fn Add an entry in pkg.dirs."
      (Conf.Value !lst)

let create ~ctxt t =
  let pkg_check pkg =
    (* TODO *)
    ()
  in
    List.iter
      (fun (pkg, fn) ->
         pkg_check pkg;
         debian_with_append_fn
           (pkg^".dirs")
           (fun chn ->
              output_string chn (fn^"\n")))
      (Conf.get ~ctxt dirs)
