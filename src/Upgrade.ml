
open Common

let init_command =
  Conf.create_full
    ~cli:"--init-command"
    (fun s -> Some s)
    "command Call this command in postinst when installing for the first time."
    (Conf.Value None)

let upgrade_command =
  Conf.create_full
    ~cli:"--upgrade-command"
    (fun s -> Some s)
    "command Call this command in postinst when upgradingi, the command may \
     contains $version and $debian_version for replacement."
    (Conf.Value None)

let create ~ctxt t =
  let get_deb_exec flag = 
    match t.deb_exec with 
      | None ->
          failwith (flag^" called without an executable package.")
      | Some {name = exec_name} ->
          exec_name
  in

  let init exec_name init_command =
    DhFiles.dh_postinst
      exec_name
      "init"
      (interpolate "\
if [ \"\$1\" = configure ] && [ \"x\$2\" = \"x\" ]; then
  $init_command
fi")
  in

  let upgrade exec_name upgrade_command =
    DhFiles.dh_postinst
      exec_name
      "upgrade"
      (interpolate "\
if [ \"\$1\" = configure ] && ! [ \"x\$2\" = \"x\" ]; then
  debian_version=\"\$2\"
  version=\"\${debian_version%-*}\"
  $upgrade_command
fi")
  in

  let init_upgrade exec_name command =
    DhFiles.dh_postinst
      exec_name
      "init-upgrade"
      (interpolate "\
if [ \"\$1\" = configure ]; then
  $command
fi")
  in

  match Conf.get ~ctxt init_command, Conf.get ~ctxt upgrade_command with
    | Some init_command, None ->
        init (get_deb_exec "--init-command") init_command

    | None, Some upgrade_command ->
        upgrade (get_deb_exec "--upgrade-command") upgrade_command

    | Some init_command, Some upgrade_command ->
        if upgrade_command = init_command then begin
          init_upgrade (get_deb_exec "--init-command") init_command
        end else begin
          let exec_name = get_deb_exec "--init-command" in
          init exec_name init_command;
          upgrade exec_name init_command
        end

    | None, None ->
        ()


