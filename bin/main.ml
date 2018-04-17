open Cmdliner

let cmds = [ Diff.cmd; Patch.cmd; ]

let main () = `Help (`Pager, None)

let cmd =
  let doc = "Xduff utility" in
  let exits = Term.default_exits in
  let man =
    [ `S Manpage.s_description
    ; `P "Xduff utility, libXdiff algorithm in OCaml." ] in
  Term.(ret (const main $ Cli.setup)),
  Term.info Cli.binary_name ~version:Cli.binary_version ~doc ~exits ~man

let () = Term.(exit @@ eval_choice cmd cmds)
