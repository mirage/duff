open Crowbar

let length l =
  List.fold_left
    (fun a -> function
      | Duff.Copy (_, len) -> a + len
      | Duff.Insert (_, len) -> a + len)
    0 l

let apply ~len:len' a b c l =
  List.fold_left
    (fun pos -> function
      | Duff.Copy (off, len) ->
          Bigstringaf.blit a ~src_off:off c ~dst_off:pos ~len;
          pos + len
      | Duff.Insert (off, len) ->
          Bigstringaf.blit b ~src_off:off c ~dst_off:pos ~len;
          pos + len)
    0 l
  |> fun len -> assert (len = len')

let () =
  add_test ~name:"duff" [ bytes; bytes ] @@ fun a b ->
  let a = Bigstringaf.of_string a ~off:0 ~len:(String.length a) in
  let b = Bigstringaf.of_string b ~off:0 ~len:(String.length b) in
  let index = Duff.make a in
  let rabin = Duff.delta index ~source:a ~target:b in
  let length = length rabin in

  if length <> Bigstringaf.length b then fail "Output length mismatch";

  let c = Bigstringaf.create length in
  apply ~len:length a b c rabin;

  check_eq
    ~pp:(Hxd_string.pp Hxd.default)
    ~eq:String.equal (Bigstringaf.to_string b) (Bigstringaf.to_string c)
