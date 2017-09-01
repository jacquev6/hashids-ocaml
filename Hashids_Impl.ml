(* Copyright 2016-2017 Vincent Jacques <vincent@vincent-jacques.net> *)

open General.Abbr
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

module ShuffleTests = struct
  open Tst

  let test = "shuffle" >:: (
    let test salt x expected =
      ~: "salt:%S x:%S -> %S" salt x expected (lazy (check_string ~expected (shuffle ~salt x)))
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

module HashTests = struct
  open Tst

  let test = "hash/unhash" >:: (
    let check alphabet n hashed =
      check_string ~expected:hashed (hash ~alphabet n);
      check_int ~expected:n (unhash ~alphabet hashed);
    in
    let make n =
      (* hash/unhash are just encoding/decoding of integer in some base when alphabet is 012...(base-1),
      so we reuse the octal, decimal and hexadecimal conversions of the Format module. *)
      ~: "%i" n (lazy (
        check "01234567" n (Frmt.apply "%o" n);
        check "0123456789" n (Frmt.apply "%i" n);
        check "0123456789abcdef" n (Frmt.apply "%x" n);
      ))
    in List.map ~f:make [0; 1; 2; 5; 7; 8; 9; 10; 11; 15; 16; 20; 31; 32; 39; 40; 63; 64]
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

module BoxTests = struct
  open Tst

  let test = "box/unbox" >:: (
    let make min_length alphabet guards seed unboxed boxed =
      ~: "%i %S %S %i %S -> %S" min_length alphabet guards seed unboxed boxed (lazy (
        check_string ~expected:boxed (box ~min_length ~alphabet ~guards ~seed unboxed);
        check_string ~expected:unboxed (unbox ~guards boxed);
      ))
    in [
      make 0 "abcd" "hij" 42 "" "";
      make 0 "abcd" "hij" 42 "vwxyz" "vwxyz";
      make 2 "abcd" "hij" 42 "vwxyz" "vwxyz";
      make 5 "abcd" "hij" 42 "vwxyz" "vwxyz";
      make 6 "abcd" "hij" 40 "vwxyz" "jvwxyz";
      make 6 "abcd" "hij" 41 "vwxyz" "hvwxyz";
      make 6 "abcd" "hij" 42 "vwxyz" "ivwxyz";
      make 7 "abcd" "hij" 40 "vwxyz" "jvwxyzh";
      make 7 "abcd" "hij" 41 "vwxyz" "hvwxyzi";
      make 7 "abcd" "hij" 42 "vwxyz" "ivwxyzj";
      make 8 "abcd" "hij" 42 "vwxyz" "civwxyzj";
      make 9 "abcd" "hij" 42 "vwxyz" "civwxyzjb";
      make 10 "abcd" "hij" 42 "vwxyz" "acivwxyzjb";
      make 11 "abcd" "hij" 42 "vwxyz" "acivwxyzjbd";
      make 12 "abcd" "hij" 42 "vwxyz" "dacivwxyzjbd";
      make 13 "abcd" "hij" 42 "vwxyz" "dacivwxyzjbda";
      make 14 "abcd" "hij" 42 "vwxyz" "cdacivwxyzjbda";
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

module EncodeTests = struct
  open Tst

  let test = "encode/decode" >:: (
    let make salt alphabet seps guards min_length xs hashid =
      let encode = encode ~salt ~alphabet ~seps ~guards ~min_length
      and decode = decode ~salt ~alphabet ~seps ~guards in
      ~: "%S %S %S %S %i -> %S" salt alphabet seps guards min_length hashid (lazy (
        check_string ~expected:hashid (encode xs);
        check_int_list ~expected:xs (decode hashid);
      ))
    in [
      "negative" >: (lazy (
        let encode = encode ~salt:"salt" ~alphabet:"abcde" ~seps:"hijk" ~guards:"xyz" ~min_length:0 in
        expect_exception
          ~expected:(Invalid_argument "negative integer (Hashids can encode only positive integers)")
          (lazy (encode [-1]))
      ));
      make "" "abcde" "hijk" "xyz" 0 [] "";
      make "" "abcde" "hijk" "xyz" 4 [] "";
      make "" "abcde" "hijk" "xyz" 0 [42] "cead";
      make "" "abcde" "hijk" "xyz" 12 [42] "daebxceadzdc";
      make "0123" "abcde" "hijk" "xyz" 0 [42] "cabd";
      make "0123" "abcde" "hijk" "xyz" 12 [42] "eecdxcabdyab";
      make "" "abcde" "hijk" "xyz" 0 [42; 57] "edeajebe";
      make "" "abcde" "hijk" "xyz" 12 [42; 57] "aezedeajebey";
      make "0123" "abcde" "hijk" "xyz" 0 [42; 57] "edabjdad";
      make "0123" "abcde" "hijk" "xyz" 12 [42; 57] "cdzedabjdady";
      make "" "abcde" "hijk" "xyz" 0 [42; 57; 72; 25] "becbjacaibebhecc";
      make "" "abcde" "hijk" "xyz" 25 [42; 57; 72; 25] "bccadxbecbjacaibebheccxeb";
      make "0123" "abcde" "hijk" "xyz" 0 [42; 57; 72; 25] "bcedjedeiacahaee";
      make "0123" "abcde" "hijk" "xyz" 25 [42; 57; 72; 25] "beedbxbcedjedeiacahaeeyac";
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

module PreprocessTests = struct
  open Tst

  let test = "preprocess" >:: (
    let make salt alphabet expected_alphabet expected_seps expected_guards =
      ~: "%S %S -> %S %S %S" salt alphabet expected_alphabet expected_seps expected_guards (lazy (
        let (alphabet, seps, guards) = preprocess ~salt ~alphabet in
        check_string ~expected:expected_alphabet alphabet;
        check_string ~expected:expected_seps seps;
        check_string ~expected:expected_guards guards;
      ))
    in [
      "too short" >: (lazy (expect_exception
        ~expected:(Invalid_argument "alphabet too short (Hashids requires at least 16 distinct characters)")
        (lazy (preprocess ~salt:"" ~alphabet:"0123456789abcde"))));
      "duplicate => too short" >: (lazy (expect_exception
        ~expected:(Invalid_argument "alphabet too short (Hashids requires at least 16 distinct characters)")
        (lazy (preprocess ~salt:"" ~alphabet:"0123456789abcdee"))));
      "space" >: (lazy (expect_exception
        ~expected:(Invalid_argument "alphabet contains space (Hashids cannot contains spaces)")
        (lazy (preprocess ~salt:"" ~alphabet:"0123456789 abcdef"))));
      make "" "0123456789abcdef" "3456789abde" "cf01" "2";
      make "" "0123456789abcdef0123456789abcdef" "3456789abde" "cf01" "2";
      make "salt" "0123456789abcdef" "8e7ab43592d" "fc01" "6";
      make "other salt" "0123456789abcdef" "93aed582764" "fc01" "b";
      "alphabet without standard separators" >:: (let alphabet = "abdegjklmnopqrvwxyzABDEGJKLMNOPQRVWXYZ1234567890" in [
        make "" alphabet "yzABDEGJKLMNOPQRVWXYZ1234567890" "abdegjklmnopqr" "vwx";
        make "salt" alphabet "zJWKwLQM7PX3Z120G6AVNB8yxR5O4Y9" "abdegjklmnopqr" "EvD";
        make "other salt" alphabet "VLXDvO2YMEw985RGB6xPQKNJz473y01" "abdegjklmnopqr" "ZAW";
      ]);
      "alphabet with one standard separator" >:: (let alphabet = "abcdegjklmnopqrvwxyzABDEGJKLMNOPQRVWXYZ1234567890" in [
        make "" alphabet "xyzABDEGJKLMNOPQRVWXYZ1234567890" "cabdegjklmnopq" "rvw";
        make "salt" alphabet "EzOJr73X9Z1wyP8K5QYVv6BGR0WA4ML2" "cabdegjklmnopq" "xND";
        make "other salt" alphabet "O7N23RL4Yz5VDZ6G10P8vK9rxwAyEWMQ" "cabdegjklmnopq" "BXJ";
      ]);
      "alphabet with almost only standard separators" >:: (let alphabet = "cfhistuCFHISTU01" in [
        make "" alphabet "01" "fhistuCFHISTU" "c";
        make "salt" alphabet "10" "iuUCFSThctfIH" "s";
        make "other salt" alphabet "10" "StIischHCuTFf" "U";
      ]);
      "alphabet with standard separators in reverse order" >:: (let alphabet = "!\"#%&',-/0123456789:;<=>ABCDEFGHIJKLMNOPQRSTUVWXYZ_`abcdefghijklmnopqrstuvwxyz~" in [
        make "" alphabet "123456789:;<=>ABDEGJKLMNOPQRVWXYZ_`abdegjklmnopqrvwxyz~" "cfhistuCFHISTU!\"#%&" "',-/0";
        make "salt" alphabet "Kj0<vLwZ'Ebo9kJe3V:Y~OaGPlMq6-7mdzxA,_`;n4WXNRy/>g2pQBr" "siuUCFSThctfIH!\"#%&" "D8=51";
        make "other salt" alphabet "Q-n8NPoWdJX'1Yzg/B;52OZLy=aqwKk4G<MVr>E,9xb:7`~6AD0R_vj" "UStIischHCuTFf!\"#%&" "lpm3e";
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


module Tests = struct
  open Tst

  let test = "Hashids_Impl" >:: [
    ShuffleTests.test;
    HashTests.test;
    BoxTests.test;
    EncodeTests.test;
    PreprocessTests.test;
  ]
end
