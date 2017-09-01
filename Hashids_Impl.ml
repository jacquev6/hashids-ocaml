(* Copyright 2016-2017 Vincent Jacques <vincent@vincent-jacques.net> *)

open Core


let shuffle x ~salt =
  if x = "" || salt = "" then x else
  let salt = salt |> String.to_array |> Array.map ~f:Char.to_int in
  (* Shuffling is based on swapping characters so we need a copy.
  Maybe we could find an implementation where each char can ben computed
  functionally and avoid mutations? *)
  let x = String.copy x in
  let swap i j = let xj = x.[j] in x.[j] <- x.[i]; x.[i] <- xj
  and x_last = String.length x - 1
  and salt_lenght = Array.length salt in
  let rec loop p = function
    | 0 -> ()
    | i ->
      let v = (x_last - i) mod salt_lenght in
      let p = p + salt.(v) in
      let j = (salt.(v) + v + p) mod i in
      swap i j;
      loop p (i - 1)
  in
  loop 0 x_last;
  x

module MakeShuffleTest(OUnit2: module type of OUnit2) = struct
  open OUnit2

  let test = "shuffle" >::: (
    let test salt x expected =
      (Printf.sprintf "salt:%S x:%S -> %S" salt x expected) >:: (fun _ -> x |> shuffle ~salt |> assert_equal ~printer:(Printf.sprintf "%S") expected)
    in [
      test "" "" "";
      test "" "abcdefghij" "abcdefghij";
      test "xyz" "" "";
      test "xyz" "a" "a";
      test "x" "abcdefghij" "bcdhfjieag";
      test "y" "abcdefghij" "eagcjhfbdi";
      test "xx" "abcdefghij" "cdhfjaiebg";
      test "yy" "abcdefghij" "fdjcghabei";
      test "xyz" "abcdefghij" "hjfacbiedg";
    ]
  )
end


let hash n ~alphabet =
  let base = String.length alphabet in
  let current n = alphabet.[n mod base]
  and next n = n / base in
  let rec loop ret = function 0 -> ret | n -> step ret n
  and step ret n = loop ((current n)::ret) (next n) in
  step [] n
  |> String.of_char_list

let unhash hashed ~alphabet =
  let base = String.length alphabet
  and digits =
    alphabet
    |> String.to_list
    |> List.mapi ~f:(fun i c -> (c, i))
    |> Char.Map.of_alist_exn
  in
  hashed
  |> String.to_list
  |> List.rev
  |> List.fold ~init:(0, 1) ~f:(fun (value, mult) c ->
    let value = value + mult * Char.Map.find_exn digits c
    and mult = base * mult in
    (value, mult)
  )
  |> Tuple.T2.get1

module MakeHashTest(OUnit2: module type of OUnit2) = struct
  open OUnit2

  let test = "hash/unhash" >::: (
    let test alphabet n hashed =
      n |> hash ~alphabet |> assert_equal ~printer:ident hashed;
      hashed |> unhash ~alphabet |> assert_equal ~printer:Int.to_string n;
    in
    let test n =
      (* hash/unhash are just encoding/decoding of integer in some base when alphabet is 012...(base-1),
      so we reuse the octal, decimal and hexadecimal conversions of the Printf module. *)
      (Printf.sprintf "%i" n) >:: (fun _ ->
        test "01234567" n (Printf.sprintf "%o" n);
        test "0123456789" n (Printf.sprintf "%i" n);
        test "0123456789abcdef" n (Printf.sprintf "%x" n);
      )
    in List.map ~f:test [0; 1; 2; 5; 7; 8; 9; 10; 11; 15; 16; 20; 31; 32; 39; 40; 63; 64]
  )
end


let box ~min_length ~alphabet ~guards ~seed =
  let guards_length = String.length guards in
  let enough hashid =
    String.length hashid >= min_length
  and guard hashid index =
    let index = (seed + Char.to_int hashid.[index]) mod guards_length in
    String.of_char guards.[index]
  in
  let guard_front hashid = guard hashid 0 ^ hashid
  and guard_back hashid = hashid ^ guard hashid 2
  and pad =
    let half = String.length alphabet / 2 in
    let rec loop alphabet hashid =
      let alphabet = shuffle ~salt:alphabet alphabet in
      let hashid = (String.drop_prefix alphabet half) ^ hashid ^ (String.prefix alphabet half) in
      if enough hashid then
        hashid
      else
        loop alphabet hashid
    in loop alphabet
  and trim hashid =
    let excess = String.length hashid - min_length in
    if excess > 0 then
      let start_pos = excess / 2 in
      String.slice hashid start_pos (start_pos + min_length)
    else
      hashid
  and if_not_enough f hashid =
    if enough hashid then
      hashid
    else
      f hashid
  in
  fun hashid ->
    hashid
    |> if_not_enough guard_front
    |> if_not_enough guard_back
    |> if_not_enough (fun hashid -> hashid |> pad |> trim)

let unbox ~guards =
  let guards = String.to_list guards in
  function hashid ->
    match String.split_on_chars ~on:guards hashid with
      | _::hashid::_ | hashid::_ -> hashid
      | [] -> failwith "String.split_on_chars returned empty list" (*BISECT-IGNORE*)

module MakeBoxTest(OUnit2: module type of OUnit2) = struct
  open OUnit2

  let test = "box/unbox" >::: (
    let test min_length alphabet guards seed unboxed boxed =
      let name = Printf.sprintf "%i %S %S %i %S -> %S" min_length alphabet guards seed unboxed boxed in
      name >:: (fun _ ->
        box ~min_length ~alphabet ~guards ~seed unboxed |> assert_equal ~printer:ident boxed;
        unbox ~guards boxed |> assert_equal ~printer:ident unboxed;
      )
    in [
      test 0 "abcd" "hij" 42 "" "";
      test 0 "abcd" "hij" 42 "vwxyz" "vwxyz";
      test 2 "abcd" "hij" 42 "vwxyz" "vwxyz";
      test 5 "abcd" "hij" 42 "vwxyz" "vwxyz";
      test 6 "abcd" "hij" 40 "vwxyz" "jvwxyz";
      test 6 "abcd" "hij" 41 "vwxyz" "hvwxyz";
      test 6 "abcd" "hij" 42 "vwxyz" "ivwxyz";
      test 7 "abcd" "hij" 40 "vwxyz" "jvwxyzh";
      test 7 "abcd" "hij" 41 "vwxyz" "hvwxyzi";
      test 7 "abcd" "hij" 42 "vwxyz" "ivwxyzj";
      test 8 "abcd" "hij" 42 "vwxyz" "civwxyzj";
      test 9 "abcd" "hij" 42 "vwxyz" "civwxyzjb";
      test 10 "abcd" "hij" 42 "vwxyz" "acivwxyzjb";
      test 11 "abcd" "hij" 42 "vwxyz" "acivwxyzjbd";
      test 12 "abcd" "hij" 42 "vwxyz" "dacivwxyzjbd";
      test 13 "abcd" "hij" 42 "vwxyz" "dacivwxyzjbda";
      test 14 "abcd" "hij" 42 "vwxyz" "cdacivwxyzjbda";
    ]
  )
end


let encode ~salt ~alphabet ~seps ~guards ~min_length =
  let alphabet_length = String.length alphabet in
  function
    | [] -> ""
    | xs ->
      let seed = List.foldi ~init:0 ~f:(fun i seed x ->
        if x < 0 then invalid_arg "negative integer (Hashids can encode only positive integers)";
        seed + x mod (i + 100)) xs
      in
      let lottery = String.of_char alphabet.[seed mod alphabet_length] in
      let (hashid, alphabet) =
        xs
        |> List.foldi ~init:(lottery, alphabet) ~f:(fun i (hashid, alphabet) x ->
          let salt = String.prefix (lottery ^ salt ^ alphabet) alphabet_length in
          let alphabet = shuffle ~salt alphabet in
          let hashed = hash x ~alphabet in
          let sep = seps.[x mod (Char.to_int hashed.[0] + i) mod (String.length seps)] in
          let hashid = hashid ^ hashed ^ (String.of_char sep) in
          (hashid, alphabet)
        )
      in
      String.drop_suffix hashid 1
      |> box ~min_length ~alphabet ~guards ~seed

let decode ~salt ~alphabet ~seps ~guards =
  let seps = String.to_list seps
  and alphabet_length = String.length alphabet
  and cut s ~i = (String.prefix s i, String.drop_prefix s i) in
  function
    | "" -> []
    | hashid ->
      let (lottery, hashid) =
        hashid
        |> unbox ~guards
        |> cut ~i:1
      in
      hashid
      |> String.split_on_chars ~on:seps
      |> List.fold ~init:([], alphabet) ~f:(fun (xs, alphabet) hashed ->
        let salt = String.prefix (lottery ^ salt ^ alphabet) alphabet_length in
        let alphabet = shuffle ~salt alphabet in
        let x = unhash hashed ~alphabet in
        let xs = x::xs in
        (xs, alphabet)
      )
      |> Tuple.T2.get1
      |> List.rev

module MakeEncodeTest(OUnit2: module type of OUnit2) = struct
  open OUnit2

  let test = "encode/decode" >::: (
    let test salt alphabet seps guards min_length xs hashid =
      let encode = encode ~salt ~alphabet ~seps ~guards ~min_length
      and decode = decode ~salt ~alphabet ~seps ~guards
      and name = Printf.sprintf "%S %S %S %S %i -> %S" salt alphabet seps guards min_length hashid in
      name >:: (fun _ ->
        xs |> encode |> assert_equal ~printer:ident hashid;
        hashid |> decode |> assert_equal ~printer:(List.to_string ~f:Int.to_string) xs;
      )
    in [
      "negative" >:: (fun _ ->
        let encode = encode ~salt:"salt" ~alphabet:"abcde" ~seps:"hijk" ~guards:"xyz" ~min_length:0 in
        assert_raises (Invalid_argument "negative integer (Hashids can encode only positive integers)") (fun () -> encode [-1])
      );
      test "" "abcde" "hijk" "xyz" 0 [] "";
      test "" "abcde" "hijk" "xyz" 4 [] "";
      test "" "abcde" "hijk" "xyz" 0 [42] "cead";
      test "" "abcde" "hijk" "xyz" 12 [42] "daebxceadzdc";
      test "0123" "abcde" "hijk" "xyz" 0 [42] "cabd";
      test "0123" "abcde" "hijk" "xyz" 12 [42] "eecdxcabdyab";
      test "" "abcde" "hijk" "xyz" 0 [42; 57] "edeajebe";
      test "" "abcde" "hijk" "xyz" 12 [42; 57] "aezedeajebey";
      test "0123" "abcde" "hijk" "xyz" 0 [42; 57] "edabjdad";
      test "0123" "abcde" "hijk" "xyz" 12 [42; 57] "cdzedabjdady";
      test "" "abcde" "hijk" "xyz" 0 [42; 57; 72; 25] "becbjacaibebhecc";
      test "" "abcde" "hijk" "xyz" 25 [42; 57; 72; 25] "bccadxbecbjacaibebheccxeb";
      test "0123" "abcde" "hijk" "xyz" 0 [42; 57; 72; 25] "bcedjedeiacahaee";
      test "0123" "abcde" "hijk" "xyz" 25 [42; 57; 72; 25] "beedbxbcedjedeiacahaeeyac";
    ]
  )
end


let preprocess =
  let all_seps = "cfhistuCFHISTU"
  and length_of_ratio ratio ~alphabet =
    Int.of_float (Float.round_up (Float.of_int (String.length alphabet) /. ratio))
  in
  let split_seps alphabet =
    (* Multiple responsibility (Hum, it's weird but still feels appropriate to do it in one single loop):
      - detect spaces
      - partition in ("not in all_seps", "in all_seps")
      - keep each char only once
      - detect if not enough chars *)
    let (alphabet, seen_seps, seen) =
      let all_seps = all_seps |> String.to_list |> Char.Set.of_list in
      String.fold alphabet ~init:([], Char.Set.empty, Char.Set.empty) ~f:(fun (alphabet, seps, seen) c ->
        if c = ' ' then invalid_arg "alphabet contains space (Hashids cannot contains spaces)";
        if Char.Set.mem seen c then (alphabet, seps, seen) else
        let seen = Char.Set.add seen c in
        if Char.Set.mem all_seps c then
          (alphabet, Char.Set.add seps c, seen)
        else
          (c::alphabet, seps, seen)
      )
    in
    if Set.length seen < 16 then
      invalid_arg "alphabet too short (Hashids requires at least 16 distinct characters)"
    else
      let alphabet =
        alphabet
        |> List.rev
        |> String.of_char_list
      and seps = String.filter all_seps ~f:(Char.Set.mem seen_seps) in
      (alphabet, seps)
  and complete_seps alphabet seps =
    let seps_min_length = length_of_ratio ~alphabet 3.5
    and seps_length = String.length seps in
    if seps_length < seps_min_length then
      (* In the Python and Java implementations, they ensure that seps_min_length >= 2
      but to have seps_min_length = 1, we need length alphabet <= 3 and since we've
      already checked that length alphabet + length seps >= 16, then we must have
      length seps >= 13 and it's impossible to be in this branch where length seps < 1. *)
      let diff = seps_min_length - seps_length in
      let seps = seps ^ (String.prefix alphabet diff)
      and alphabet = String.drop_prefix alphabet diff in
      (alphabet, seps)
    else
      (alphabet, seps)
  and make_guards alphabet seps =
    let guards_length = length_of_ratio ~alphabet 12. in
    if String.length alphabet < 3 then
      let guards = String.prefix seps guards_length
      and seps = String.drop_prefix seps guards_length
      in (alphabet, seps, guards)
    else
      let guards = String.prefix alphabet guards_length
      and alphabet = String.drop_prefix alphabet guards_length
      in (alphabet, seps, guards)
  in
  fun ~salt ~alphabet ->
    let (alphabet, seps) = split_seps alphabet in
    let seps = shuffle seps ~salt in
    let (alphabet, seps) = complete_seps alphabet seps in
    let alphabet = shuffle alphabet ~salt in
    make_guards alphabet seps

module MakePreprocessTest(OUnit2: module type of OUnit2) = struct
  open OUnit2

  let test = "preprocess" >::: (
    let test salt alphabet expected_alphabet expected_seps expected_guards =
      let name = Printf.sprintf "%S %S -> %S %S %S" salt alphabet expected_alphabet expected_seps expected_guards in
      name >:: (fun _ ->
        let (alphabet, seps, guards) = preprocess ~salt ~alphabet in
        assert_equal ~printer:ident expected_alphabet alphabet;
        assert_equal ~printer:ident expected_seps seps;
        assert_equal ~printer:ident expected_guards guards;
      )
    in [
      "too short" >:: (fun _ -> assert_raises (Invalid_argument "alphabet too short (Hashids requires at least 16 distinct characters)") (fun () -> preprocess ~salt:"" ~alphabet:"0123456789abcde"));
      "duplicate => too short" >:: (fun _ -> assert_raises (Invalid_argument "alphabet too short (Hashids requires at least 16 distinct characters)") (fun () -> preprocess ~salt:"" ~alphabet:"0123456789abcdee"));
      "space" >:: (fun _ -> assert_raises (Invalid_argument "alphabet contains space (Hashids cannot contains spaces)") (fun () -> preprocess ~salt:"" ~alphabet:"0123456789 abcdef"));
      test "" "0123456789abcdef" "3456789abde" "cf01" "2";
      test "" "0123456789abcdef0123456789abcdef" "3456789abde" "cf01" "2";
      test "salt" "0123456789abcdef" "8e7ab43592d" "fc01" "6";
      test "other salt" "0123456789abcdef" "93aed582764" "fc01" "b";
      "alphabet without standard separators" >::: (let alphabet = "abdegjklmnopqrvwxyzABDEGJKLMNOPQRVWXYZ1234567890" in [
        test "" alphabet "yzABDEGJKLMNOPQRVWXYZ1234567890" "abdegjklmnopqr" "vwx";
        test "salt" alphabet "zJWKwLQM7PX3Z120G6AVNB8yxR5O4Y9" "abdegjklmnopqr" "EvD";
        test "other salt" alphabet "VLXDvO2YMEw985RGB6xPQKNJz473y01" "abdegjklmnopqr" "ZAW";
      ]);
      "alphabet with one standard separator" >::: (let alphabet = "abcdegjklmnopqrvwxyzABDEGJKLMNOPQRVWXYZ1234567890" in [
        test "" alphabet "xyzABDEGJKLMNOPQRVWXYZ1234567890" "cabdegjklmnopq" "rvw";
        test "salt" alphabet "EzOJr73X9Z1wyP8K5QYVv6BGR0WA4ML2" "cabdegjklmnopq" "xND";
        test "other salt" alphabet "O7N23RL4Yz5VDZ6G10P8vK9rxwAyEWMQ" "cabdegjklmnopq" "BXJ";
      ]);
      "alphabet with almost only standard separators" >::: (let alphabet = "cfhistuCFHISTU01" in [
        test "" alphabet "01" "fhistuCFHISTU" "c";
        test "salt" alphabet "10" "iuUCFSThctfIH" "s";
        test "other salt" alphabet "10" "StIischHCuTFf" "U";
      ]);
      "alphabet with standard separators in reverse order" >::: (let alphabet = "!\"#%&',-/0123456789:;<=>ABCDEFGHIJKLMNOPQRSTUVWXYZ_`abcdefghijklmnopqrstuvwxyz~" in [
        test "" alphabet "123456789:;<=>ABDEGJKLMNOPQRVWXYZ_`abdegjklmnopqrvwxyz~" "cfhistuCFHISTU!\"#%&" "',-/0";
        test "salt" alphabet "Kj0<vLwZ'Ebo9kJe3V:Y~OaGPlMq6-7mdzxA,_`;n4WXNRy/>g2pQBr" "siuUCFSThctfIH!\"#%&" "D8=51";
        test "other salt" alphabet "Q-n8NPoWdJX'1Yzg/B;52OZLy=aqwKk4G<MVr>E,9xb:7`~6AD0R_vj" "UStIischHCuTFf!\"#%&" "lpm3e";
      ]);
    ]
  )
end


type t = {
  encode: int list -> string;
  decode: string -> int list;
}

let make ?(salt="") ?(min_length=0) ?(alphabet="abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890") () =
  let min_length = Int.max 0 min_length
  and (alphabet, seps, guards) = preprocess ~salt ~alphabet in
  let encode = encode ~salt ~alphabet ~seps ~min_length ~guards
  and decode = decode ~salt ~alphabet ~seps ~guards in
  {encode; decode}


module MakeTest(OUnit2: module type of OUnit2) = struct
  open OUnit2

  let test = "Hashids_Impl" >::: [
    (let module Test = MakeShuffleTest(OUnit2) in Test.test);
    (let module Test = MakeHashTest(OUnit2) in Test.test);
    (let module Test = MakeBoxTest(OUnit2) in Test.test);
    (let module Test = MakeEncodeTest(OUnit2) in Test.test);
    (let module Test = MakePreprocessTest(OUnit2) in Test.test);
  ]
end
