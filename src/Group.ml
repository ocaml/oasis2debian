
open Common

let group = 
  Conf.create_full
    ~cli:"--group"
    (fun s ->
       Scanf.sscanf
         s "%s@,%s"
         (fun group homedir ->
            Some (group, homedir)))
    "group,homedir Create a group for the executable package (postinst/prerm)."
    (Conf.Value None)

let create ~ctxt t = 
  match Conf.get ~ctxt group, t.deb_exec with 
    | Some (group, homedir), Some {name = exec_name} ->
        let snippet_name = Printf.sprintf "group(%s)" group in
          DhFiles.dh_dirs
            exec_name
            homedir;
          DhFiles.dh_postinst
            exec_name
            snippet_name
            (interpolate "\
if [ \"\$1\" = configure ]; then
  if ! getent group '$group' > /dev/null; then
    adduser --system --quiet --home '$homedir' --no-create-home \\
      --disabled-password --group '$group'
  fi
fi");
          DhFiles.dh_prerm
            exec_name
            snippet_name
            (interpolate "\
if [ \"\$1\" = remove ]; then
  delgroup '$group' > /dev/null 2>&1 || true
fi")

    | Some _, None ->
        failwith "--group without an executable package."

    | None, _ ->
        ()
