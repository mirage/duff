let load_file filename =
  let bytes = Bytes.create 0xFF in
  let buffer = Buffer.create 0xFF in

  Bos.OS.File.with_input ~bytes filename (fun input buffer ->
      let rec go () = match input () with
        | Some (tmp, off, len) ->
          Buffer.add_subbytes buffer tmp off len; go ()
        | None -> () in go ()) buffer
  |> function
  | Ok () -> Buffer.contents buffer |> Cstruct.of_string
  | Error (`Msg err) -> invalid_arg err

let set_byte bytes off value = Bytes.unsafe_set bytes off (Char.unsafe_chr value)

let serialize_one bytes target = function
  | Duff.Copy (off, len) ->
    let pos = ref 1 in
    let stp = ref 0 in

    if off land 0x000000ff <> 0
    then begin set_byte bytes !pos (off lsr  0); incr pos; stp := !stp lor 0x01; end;
    if off land 0x0000ff00 <> 0
    then begin set_byte bytes !pos (off lsr  8); incr pos; stp := !stp lor 0x02; end;
    if off land 0x00ff0000 <> 0
    then begin set_byte bytes !pos (off lsr 16); incr pos; stp := !stp lor 0x04; end;
    if off land 0xff000000 <> 0
    then begin set_byte bytes !pos (off lsr 24); incr pos; stp := !stp lor 0x08 end;

    if len land 0x00ff <> 0
    then begin set_byte bytes !pos (len lsr  0); incr pos; stp := !stp lor 0x10; end;
    if len land 0xff00 <> 0
    then begin set_byte bytes !pos (len lsr  8); incr pos; stp := !stp lor 0x20; end;

    set_byte bytes 0 (0x80 lor !stp);

    (bytes, 0, !pos)
  | Duff.Insert (off, len) ->
    set_byte bytes 0 len;
    Cstruct.blit_to_bytes target off bytes 1 len;

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

module Diff = Duff.Default

let diff source target =
  let source_content = load_file source in
  let target_content = load_file target in

  let index = Diff.Index.make source_content in

  let rabin = Diff.delta index target_content in
  let bytes = Bytes.create 0x80 in

  serialize bytes target_content stdout rabin
  |> function
  | Ok _ -> Ok ()
  | Error _ as err -> err

module Decode =
struct
  type error

  type t =
    { i_off : int
    ; i_pos : int
    ; i_len : int
    ; slice : Bytes.t
    ; state : state }
  and state =
    | O
    | K of (Bytes.t -> t -> res)
    | R of hunk
  and res =
    | Cont of t
    | Wait of t
    | Retn of t * hunk
  and hunk =
    | C of { off : int
           ; len : int; }
    | I of { tmp : Bytes.t
           ; len : int; }

  let get_byte k src t =
    if (t.i_len - t.i_pos) > 0
    then let byte = Char.code (Bytes.get src (t.i_off + t.i_pos)) in
      k byte src { t with i_pos = t.i_pos + 1 }
    else Wait t

  let get_byte_if test k src t =
    if not (test <> 0)
    then k 0 src t
    else if test <> 0 && (t.i_len - t.i_pos) > 0
    then let byte = Char.code (Bytes.get src (t.i_off + t.i_pos)) in
      k byte src { t with i_pos = t.i_pos + 1 }
    else Wait t

  let rec get_slice ?(off = 0) len src t =
    if len = 0
    then Cont { t with state = (R (I { tmp = t.slice; len = off; })) }
    else if (t.i_len - t.i_pos) > 0
    then
      begin
        let n = min len (t.i_len - t.i_pos) in

        Bytes.blit src (t.i_off + t.i_pos) t.slice off n;

        Cont { t with state = K (get_slice ~off:(off + n) (len - n))
                    ; i_pos = t.i_pos + n }
      end
    else Wait t

  let code byte src t = match byte land 0x80 with
    | 0x80 ->
      let k off len _src t =
        Cont { t with state = (R (C { off; len = if len = 0 then 0x10000 else len })) } in

      (get_byte_if (byte land 0x01)
       @@ fun o0 -> get_byte_if (byte land 0x02)
       @@ fun o1 -> get_byte_if (byte land 0x04)
       @@ fun o2 -> get_byte_if (byte land 0x08)
       @@ fun o3 -> get_byte_if (byte land 0x10)
       @@ fun l0 -> get_byte_if (byte land 0x20)
       @@ fun l1 -> get_byte_if (byte land 0x40)
       @@ fun l2 -> k
         ((o3 lsl 24) lor (o2 lsl 16) lor (o1 lsl 8) lor o0)
         ((l2 lsl 16) lor (l1 lsl 8) lor l0))
        src t
    | _ ->
      get_slice byte src t

  let o src t = get_byte code src t

  let eval src t =
    let eval0 t = match t.state with
      | K k -> k src t
      | O   -> o src t
      | R x -> Retn (t, x) in
    let rec loop t = match eval0 t with
      | Cont t -> loop t
      | Wait t -> `Wait t
      | Retn (t, x) -> `Hunk (t, x) in
    loop t

  let continue t = { t with state = O }

  let refill off len t =
    { t with i_off = off
           ; i_len = len
           ; i_pos = 0 }

  let default =
    { i_off = 0
    ; i_pos = 0
    ; i_len = 0
    ; slice = Bytes.create 0x1000
    ; state = O }
end

let reconstruct source =
  let source_content = load_file source in
  let insert = Bytes.create 0x1000 in
  let bytes = Bytes.create 0x1000 in

  let with_input_and_output ic oc =
    let output = function
      | Some (tmp, off, len) -> output_substring oc (Bytes.unsafe_to_string tmp) off len
      | None -> () in

    let input () = match input ic bytes 0 (Bytes.length bytes) with
      | 0 -> None
      | n -> Some (bytes, 0, n)
      | exception End_of_file -> None in

    let rec go bytes t = match Decode.eval bytes t with
      | `Hunk (t, Decode.I { tmp; len; }) ->
        output (Some (tmp, 0, len));
        go bytes (Decode.continue t)
      | `Hunk (t, Decode.C { off; len; }) ->
        Cstruct.blit_to_bytes source_content off insert 0 len;
        output (Some (insert, 0, len));
        go bytes (Decode.continue t)
      | `Wait t -> match input () with
        | Some (bytes, off, len) -> go bytes (Decode.refill off len t)
        | None -> Ok () in
    go bytes Decode.default in

  with_input_and_output stdin stdout
  |> function
  | Ok _ -> Ok ()
  | Error _ as err -> err

open Cmdliner

let main source target action = match target, action with
  | Some target, `Diff ->
    let source = Fpath.v source in
    let target = Fpath.v target in
    diff source target
  | None, `Patch ->
    let source = Fpath.v source in
    reconstruct source
  | _ -> assert false (* TODO *)

let source =
  let doc = "Source file" in
  Arg.(required & pos 1 (some file) None & info [] ~docv:"SOURCE" ~doc)

let target =
  let doc = "Target file" in
  Arg.(value & pos 2 (some file) None & info [] ~docv:"TARGET" ~doc)

let diff =
  let doc = "Compute diff or patch" in
  let diff_or_patch = Arg.enum [ "diff", `Diff; "patch", `Patch ] in
  Arg.(required & pos 0 (some diff_or_patch) None & info [] ~doc)

let cmd =
  let doc = "Diff files" in
  let exits = Term.default_exits in
  let man =
    [ `S Manpage.s_description
    ; `P "Generate an xduff file which is a thin representation of $(i,TARGET) from $(i,SOURCE)." ] in
  Term.(const main $ source $ target $ diff),
  Term.info "xduff" ~version:"0.1" ~doc ~exits ~man

let () = Term.(exit @@ eval cmd)
