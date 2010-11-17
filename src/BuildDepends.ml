
open OASISTypes
open OASISUtils
open OASISMessage
open Common
open ExtString
open FileUtil
open FilePath

type arch = 
  | ArchAll
  | ArchOnly of string * string list

module SetExec = 
  Set.Make
    (String)

module SetFindlib = 
  Set.Make
    (struct
       type t = findlib_full * OASISVersion.comparator option
       let compare = Pervasives.compare
     end)

let all_confs = 
  let linux_i386 = 
    []
  in

  let linux_amd64 = 
    []
  in

    [
      "i386", linux_i386;
      "amd64", linux_amd64;
(*
      "alpha", linux_alpha;
      "armel", linux_armel;
      "hppa", linux_hppa;
      "hurd-i386", hurd_i386;
      "ia64", linux_ia64;
      "kfreebsd-amd64", kfreebsd_amd64;
      "kfreebsd-i386", kfreebsd_i386;
      "mips", linux_mips;
      "mipsel", linux_mipsel;
      "powerpc", linux_powerpc;
      "s390", linux_s390;
      "sparc", linux_sparc;
 *)
    ]

module SetDepends =
  Set.Make
    (struct
       type t = string * OASISVersion.comparator option * arch 
       let compare (nm1, _, _) (nm2, _, _) = 
         String.compare nm1 nm2
     end)

let add_depends ?arch nm ver_opt st =
  let ver_opt', arch' =
    try 
      let _, ver_opt', arch' = 
        List.find
          (fun (nm', _, _) -> nm' = nm)
          (SetDepends.elements st)
      in
        ver_opt', Some arch'

    with Not_found ->
      None, None
  in

  let ver_opt = 
    match ver_opt, ver_opt' with
      | Some v1, Some v2 ->
          Some 
            (OASISVersion.comparator_reduce 
               (OASISVersion.VAnd (v1, v2)))
      | None, opt
      | opt, _ ->
          opt
  in

  let arch =
    match arch, arch' with
      | Some a, None -> 
          ArchOnly (a, [])

      | Some _, Some ArchAll ->
          ArchAll

      | Some a, Some (ArchOnly (hd, tl)) ->
          if not (List.mem a (hd :: tl)) then
            ArchOnly (hd, a :: tl)
          else
            ArchOnly (hd, tl)

      | None, _ ->
          ArchAll
  in

    SetDepends.add (nm, ver_opt, arch) st

let string_of_depends (nm, ver_opt, arch) = 
  let arch_str = 
    match arch with 
      | ArchAll -> ""
      | ArchOnly (hd, tl) ->
          begin
            (* All arches of the package *)
            let lst = hd :: tl in 
            let neg =
              List.map ((^) "!") 
                (List.filter 
                   (* Remove arches of the package *)
                   (fun nm -> not (List.mem nm lst)) 
                   (* All arches *)
                   (List.map fst all_confs))
            in

            let lst =
              if List.length neg < List.length lst then
                neg
              else
                lst
            in            
              Printf.sprintf " [%s]" (String.concat ", " lst)
          end
  in
  let ver_str =
    match ver_opt with 
      | Some v ->
          Printf.sprintf " (%s)"
            (OASISVersion.string_of_comparator v)
      | None ->
          ""
  in
    nm^ver_str^arch_str

(* Compute build dependencies, against real debian packages
 *)
let get ~ctxt pkg = 

  let depends_of_conf conf acc = 
    let eval conf = 
      OASISExpr.choose
        (fun nm -> 
           try 
             List.assoc nm conf
           with Not_found as e ->
             error ~ctxt "OASIS variable '%s' not defined" nm;
             raise e)
    in

    let conf = 
      (* Evaluate flag values and add then to conf *)
      List.fold_left 
        (fun conf ->
           function 
             | Flag (cs, flag) ->
                 (cs.cs_name, 
                  string_of_bool (eval conf flag.flag_default))
                 :: conf
             | Library _ | Executable _ | Doc _ | Test _ | SrcRepo _ ->
                 conf)
        conf
        pkg.sections
    in

    let depends_of_bs bs ((exec, fndlb) as acc) =
      if eval conf bs.bs_build && eval conf bs.bs_install then
        begin
          let exec = 
            List.fold_left 
              (fun exec ->
                 function
                   | ExternalTool nm -> 
                       SetExec.add nm exec
                   | InternalExecutable _ ->
                       exec)
              exec
              bs.bs_build_tools
          in

          let fndlb =
            List.fold_left
              (fun fndlb ->
                 function
                   | FindlibPackage (nm, ver_opt) ->
                       SetFindlib.add (nm, ver_opt) fndlb
                   | InternalLibrary _ ->
                       fndlb)
              fndlb
              bs.bs_build_depends
          in

            exec, fndlb
        end
      else
        begin
          acc
        end
    in

      List.fold_left 
        (fun ((exec, fndlb) as acc) ->
           function
             | Library (_, bs, _) 
             | Executable (_, bs, _) ->
                 depends_of_bs bs acc
             
             | Flag _ | Test _ | SrcRepo _ | Doc _ ->
                 acc)
        acc
        pkg.sections
  in

  let ocaml_stdlib_dir = 
    match assert_command_output ~ctxt "ocamlc -where" with
      | hd :: _ ->
          hd
      | [] ->
          failwith
            "Cannot determine ocaml standard library directory"
  in

  let debian_depends_of_depends ?arch exec fndlb st = 
    (* Find file *)
    let find_file ?(dev_pkg=false) fn = 
      let filter lst = 
        if dev_pkg then
          begin
            let lst' = 
              List.filter
                (fun nm -> String.ends_with nm "-dev")
                lst
            in
              (* Some package don't adhere to the -dev convention (e.g. camlp4),
               * so better return something than nothing
               *)
              if lst' <> [] then
                lst'
              else 
                lst
          end
        else
          lst
      in

      let output = 
        try 
          assert_command_output ~ctxt
            (Printf.sprintf "dpkg -S '%s'" fn)
        with _ ->        
          assert_command_output ~ctxt 
            (Printf.sprintf "apt-file search -F '%s'" fn)
      in

      let pkg = 
        List.flatten 
          (List.rev_map 
             (fun line ->
                match String.nsplit line ":" with 
                  | hd :: _ -> 
                      List.rev_map 
                        String.strip 
                        (String.nsplit hd ",")
                  | _ ->
                      [])
             output)
      in

        match filter pkg with 
          | hd :: tl ->
              if tl <> [] then
                warning ~ctxt 
                  "Choose package '%s' but other packages possible: %s"
                  hd 
                  (String.concat ", " 
                     (List.map 
                        (Printf.sprintf "'%s'") 
                        tl));
              hd

          | [] ->
              failwithf1
                "Cannot find file '%s'"
                fn
    in

    (* Find an executable *)
    let find_exec nm = 
      let fn = 
        try 
          which 
            ~path:["/usr/bin"; "/bin"; "/usr/sbin"; "/sbin"]
            nm 
        with Not_found ->
          (* Make a guess *)
          make_filename ["/usr/bin"; nm]
      in
        find_file fn
    in

    (* Find a findlib library *)
    let find_findlib nm =
      let fn = 
        try 
          let parse_output =
            function
              | [] | [""] ->
                  raise Not_found
              | hd :: _ -> 
                  hd
          in

          let archive pred = 
            let output = 
              assert_command_output ~ctxt
                (Printf.sprintf 
                   "ocamlfind query -predicates '%s' %s -format '%%d/%%a'"
                   pred nm)
            in
              parse_output output
          in

            try 
              begin
                archive "byte" 
              end

            with Not_found ->
              begin
                try
                  archive "syntax,toploop"
                with Not_found ->
                  begin
                    let output = 
                      assert_command_output ~ctxt 
                        (Printf.sprintf
                           "ocamlfind query %s -format '%%d/META'"
                           nm)
                    in
                    let fn =
                      parse_output output 
                    in
                      if Sys.file_exists fn then
                        fn
                      else
                        begin
                          warning ~ctxt 
                            "Cannot find installed file for findlib package '%s'" 
                            nm;
                          raise Not_found 
                        end
                  end
              end

        with _ ->
          (* Make a guess *)
          make_filename [ocaml_stdlib_dir; nm]
      in
        find_file ~dev_pkg:true fn
    in

    let st = 
      SetExec.fold
        (fun nm st ->
           add_depends ?arch (find_exec nm) None st)
        exec
        st
    in

      SetFindlib.fold
        (fun (nm, ver_opt) st ->
           add_depends ?arch (find_findlib nm) ver_opt st)
        fndlb
        st
  in

  let lst = 
    List.map
      (fun (nm, conf) ->
         nm, 
         depends_of_conf 
           conf 
           (SetExec.empty, SetFindlib.empty))
      all_confs
  in

  (* Compute common dependencies *)
  let common_exec, common_fndlb = 
    match lst with 
      | (_, hd) :: tl ->
          List.fold_left
            (fun (exec, fndlb) (_, (exec', fndlb')) ->
               SetExec.inter exec exec',
               SetFindlib.inter fndlb fndlb')
            hd tl
      | _ ->
          SetExec.empty,
          SetFindlib.empty
  in

  let debian_depends = 
    List.fold_right 
      SetDepends.add 
      [
        "ocaml-findlib", pkg.findlib_version, ArchAll;
        "ocaml-nox", pkg.ocaml_version, ArchAll;
      ]
      SetDepends.empty
  in

  let debian_depends = 
    debian_depends_of_depends 
      common_exec
      common_fndlb
      debian_depends
  in


  (* Check for extra dependencies for particular architecture *)
  let debian_depends = 
    List.fold_left 
      (fun st (arch, (exec, fndlb)) ->
         let exec'  = SetExec.diff exec common_exec in
         let fndlb' = SetFindlib.diff fndlb common_fndlb in
           debian_depends_of_depends
             ~arch
             exec'
             fndlb'
             st)
      debian_depends
      lst
  in

    (* Translate depends into string *)
    SetDepends.fold
      (fun dep lst ->
         string_of_depends dep :: lst)
      debian_depends

