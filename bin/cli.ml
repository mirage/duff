open Cmdliner

[@@@warning "-32"]

let binary_name = "xduff"
let binary_version = "1.0"

let path_arg = Arg.conv Fpath.(of_string, pp)

let binary_name = "xduff"
let version = "0.1"

let main style_renderer log_level cwd =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level log_level;
  Logs.set_reporter (Logs_fmt.reporter ~app:Fmt.stdout ());
  Logs.info (fun m -> m "xduff running");
  match cwd with
  | None -> `Ok ()
  | Some dir ->
    match Bos.OS.Dir.set_current dir with
    | Ok () -> `Ok ()
    | Error (`Msg err) -> `Error (false, err)

let setup =
  let style_renderer =
    let env = Arg.env_var "XDUFF_COLOR" in
    Fmt_cli.style_renderer ~docs:Manpage.s_common_options ~env () in
  let log_level =
    let env = Arg.env_var "XDUFF_VERBOSITY" in
    Logs_cli.level ~docs:Manpage.s_common_options ~env () in
  let cwd =
    let doc = "Change directory $(docv) before doing anything." in
    let docv = "DIR" in
    Arg.(value & opt (some path_arg) None & info ["C"; "dir"] ~docs:Manpage.s_common_options ~doc ~docv) in
  Term.(ret (const main $ style_renderer $ log_level $ cwd))
