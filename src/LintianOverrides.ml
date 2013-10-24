
open Common

let no_manpage =
  let lst = ref [] in
    Conf.create_full
      ~cli:"--no-manpage"
      (fun s -> lst := s :: !lst; !lst)
      "exec Disable lintian warning for the given exec without a manpage."
      (Conf.Value !lst)

let add pkg lintian_id arg_opt =
  debian_with_append_fn
    (pkg^".lintian-overrides")
    (output_content
       (match arg_opt with
          | Some arg -> Printf.sprintf "%s %s" lintian_id arg
          | None -> lintian_id))

let create ~ctxt t =
  List.iter
    (fun arg ->
       match t.deb_exec with
         | Some {Common.name = pkg} ->
             add pkg "binary-without-manpage" (Some arg)
         | None ->
             failwith "--no-manpage require a -bin package.")
    (Conf.get ~ctxt no_manpage);
  if not (Conf.is_set Changelog.itp) then
    begin
      (* TODO: don't do that if the package is not new. *)
      let packages =
        List.flatten
          [
            begin
              match t.deb_exec with
                | Some {Common.name = name} -> [name]
                | None -> []
            end;

            begin
              match t.deb_dev with
                | Some ({Common.name = name_dev},
                        {Common.name = name_runtime}) ->
                    [name_dev; name_runtime]
                | None ->
                    []
            end;

            begin
              match t.deb_doc with
                | Some {Common.name = name} -> [name]
                | None -> []
            end;
          ]
      in
        List.iter
          (fun pkg ->
             add pkg "new-package-should-close-itp-bug" None)
          packages
    end
