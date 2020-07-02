open Utils

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

        Bytes.blit src (t.i_off + t.i_pos) t.slice off n ;

        Cont { t with state = K (get_slice ~off:(off + n) (len - n))
                    ; i_pos = t.i_pos + n }
      end
    else Wait t

  let code byte src t = match byte land 0x80 with
    | 0x80 ->
      let k off len _src t =
        Cont { t with state = (R (C { off; len = if len = 0 then 0x10000 else len })) } in

      ( get_byte_if (byte land 0x01)
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

let patch source =
  let source_content = load_file source in
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
        Fmt.epr ">>> Insert %S.\n%!" (Bytes.sub_string tmp 0 len) ;
        output (Some (tmp, 0, len));
        go bytes (Decode.continue t)
      | `Hunk (t, Decode.C { off; len; }) ->
        Fmt.epr ">>> Copy off:%d, len:%d.\n%!" off len ;
        let insert = Bigstringaf.(to_string (sub source_content ~off ~len)) in
        output (Some (Bytes.unsafe_of_string insert, 0, len)) ;
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

let source =
  let doc = "Source file" in
  Arg.(required & pos 0 (some Cli.path_arg) None & info [] ~docv:"SOURCE" ~doc)

let cmd =
  let doc = "Patch files" in
  let exits = Term.default_exits in
  let man =
    [ `S Manpage.s_description
    ; `P "Reconstruct an xduff input from $([,SOURCE])."] in
  Term.(const patch $ source),
  Term.info "patch" ~version:Cli.binary_version ~doc ~exits ~man

