external random_seed : unit -> int array = "caml_sys_random_seed"

let seed = random_seed ()
let () = Fmt.pr "seed: %a.\n%!" Fmt.(Dump.array int) seed
let () = Random.full_init seed

let random_string length = String.init length (fun _ -> Char.chr (Random.int 0x100))

let apply a b l r =
  let c = Bytes.create l in
  let _ = List.fold_left
      (fun pos -> function
         | Duff.Copy (off, len) ->
           Bytes.blit_string a off c pos len;
           pos + len
         | Duff.Insert (off, len) ->
           Bytes.blit_string b off c pos len;
           pos + len) 0 r in
  Bytes.unsafe_to_string c

let test length =
  Alcotest.test_case (Fmt.strf "random:%d" length) `Quick
  @@ fun () ->
  let a = random_string length in
  let b = random_string length in
  let index = Duff.Default.Index.make (Cstruct.of_string a) in
  let rabin = Duff.Default.delta index (Cstruct.of_string b) in
  let length' = List.fold_left (fun a -> function
      | Duff.Copy (_, len) -> a + len
      | Duff.Insert (_, len) -> a + len)
      0 rabin in

  Alcotest.(check int) "length" length length';
  Alcotest.(check string) "apply" b (apply a b length rabin)

let list_init f n =
  let rec go acc = function
    | 0 -> List.rev acc
    | n -> go (f n :: acc) (pred n) in
  go [] n

let tests () =
  Alcotest.run "duff"
    [ "random iso", list_init (fun _ -> test 1024) 64 ]

let () = tests ()
