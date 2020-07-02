open Utils

let set_byte bytes off value = Bytes.unsafe_set bytes off (Char.unsafe_chr value)

let serialize_one bytes target = function
  | Duff.Copy (off, len) ->
    let pos = ref 1 in
    let stp = ref 0 in

    if off land 0x000000ff <> 0
    then begin set_byte bytes !pos (off lsr  0); incr pos; stp := !stp lor 0x01; end;
    if off land 0x0000ff00 <> 0
    then begin set_byte bytes !pos (off lsr  8) ; incr pos ; stp := !stp lor 0x02 end ;
    if off land 0x00ff0000 <> 0
    then begin set_byte bytes !pos (off lsr 16) ; incr pos ; stp := !stp lor 0x04 end ;
    if off land 0xff000000 <> 0
    then begin set_byte bytes !pos (off lsr 24) ; incr pos ; stp := !stp lor 0x08 end ;

    if len land 0x00ff <> 0
    then begin set_byte bytes !pos (len lsr  0) ; incr pos ; stp := !stp lor 0x10 end ;
    if len land 0xff00 <> 0
    then begin set_byte bytes !pos (len lsr  8) ; incr pos ; stp := !stp lor 0x20 end ;

    set_byte bytes 0 (0x80 lor !stp) ;

    (bytes, 0, !pos)
  | Duff.Insert (off, len) ->
    set_byte bytes 0 len;
    Bigstringaf.blit_to_bytes target ~src_off:off bytes ~dst_off:1 ~len ;

    (bytes, 0, len + 1)

let (<.>) f g = fun x -> f (g x)
let some x = Some x

let serialize bytes target oc rabin =
  let output = function
    | Some (tmp, off, len) -> output_substring oc (Bytes.unsafe_to_string tmp) off len
    | None -> () in

  List.iter (output <.> some <.> serialize_one bytes target) rabin;
  output None;
  Ok ()

let diff source target =
  let source_content = load_file source in
  let target_content = load_file target in

  let index = Duff.make source_content in

  let rabin = Duff.delta index ~source:source_content ~target:target_content in
  let bytes = Bytes.create 0x80 in

  serialize bytes target_content stdout rabin
  |> function
  | Ok _ -> Ok ()
  | Error _ as err -> err

open Cmdliner

let source =
  let doc = "Source file" in
  Arg.(required & pos 0 (some Cli.path_arg) None & info [] ~docv:"SOURCE" ~doc)

let target =
  let doc = "Target file" in
  Arg.(required & pos 1 (some Cli.path_arg) None & info [] ~docv:"TARGET" ~doc)

let cmd =
  let doc = "Diff files" in
  let exits = Term.default_exits in
  let man =
    [ `S Manpage.s_description
    ; `P "Generate an xduff file which is a thin representation of $(i,TARGET) from $(i,SOURCE)" ] in
  Term.(const diff $ source $ target),
  Term.info "diff" ~version:Cli.binary_version ~doc ~exits ~man
