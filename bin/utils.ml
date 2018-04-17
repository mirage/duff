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

