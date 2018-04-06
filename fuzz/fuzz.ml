open Crowbar

let length l =
  List.fold_left
    (fun a -> function
      | Duff.Copy (_, len) -> a + len
      | Duff.Insert (_, len) -> a + len)
    0 l

let apply a b c l =
  List.fold_left
    (fun pos -> function
      | Duff.Copy (off, len) ->
         Cstruct.blit a off c pos len;
         pos + len
      | Duff.Insert (off, len) ->
         Cstruct.blit b off c pos len;
         pos + len)
    0 l |> fun _len -> ()

let () =
  add_test
    ~name:"duff"
    [ bytes; bytes ]
  @@ fun a b ->
     let a = Cstruct.of_string a in
     let b = Cstruct.of_string b in
     let index = Duff.Default.Index.make a in
     let rabin = Duff.Default.delta index b in

     let length = length rabin in

     if length <> Cstruct.len b
     then fail "Output length mismatch";

     let c = Cstruct.create length in
     apply a b c rabin;

     check_eq ~pp:Cstruct.hexdump_pp ~eq:Cstruct.equal b c
