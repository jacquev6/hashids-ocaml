(* Copyright 2016-2017 Vincent Jacques <vincent@vincent-jacques.net> *)

open Core
open OUnit2


let test = "Hashids" >::: [
  (let module Test = Hashids_Impl.MakeTest(OUnit2) in Test.test);
  "public interface" >::: [
    "alphabet too short" >:: (fun _ -> assert_raises
      (Invalid_argument "alphabet too short (Hashids requires at least 16 distinct characters)")
      (fun () -> Hashids.make ~alphabet:"abcdefghijklmno" ())
    );
    "negative" >:: (fun _ ->
      let {Hashids.encode; _} = Hashids.make () in
      assert_raises
      (Invalid_argument "negative integer (Hashids can encode only positive integers)")
      (fun () -> encode [-1])
    );
    "encode/decode" >::: (
      let success name ?salt ?min_length ?alphabet xs encoded =
        let {Hashids.encode; decode} = Hashids.make ?salt ?min_length ?alphabet () in
        (name ^ ": " ^ encoded) >:: (fun _ ->
          xs |> encode |> assert_equal ~printer:ident encoded;
          encoded |> decode |> assert_equal ~printer:(List.to_string ~f:Int.to_string) xs;
        )
      in [
        success "empty" [] "";
        success "default" [0] "gY";
        success "default" [1] "jR";
        success "default" [22] "Lw";
        success "default" [333] "Z0E";
        success "default" [9999] "w0rR";
        success "default" [12345] "j0gW";
        success "default" [1; 2; 3] "o2fXhV";
        success "default" [683; 94108; 123; 5] "vJvi7On9cXGtD";
        success "default" [2; 4; 6] "xGhmsW";
        success "default" [99; 25] "3lKfD";
        success "default" [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20; 21; 22; 23; 24] "3RcpfqhKi5s0t0u6CQFKHRIqS0TPU2cofrhRi8sjtLuzCGFoHE";
        success "salt" ~salt:"this is my salt" [1] "NV";
        success "salt" ~salt:"this is my salt" [2] "6m";
        success "salt" ~salt:"this is my salt" [3] "yD";
        success "salt" ~salt:"this is my salt" [4] "2l";
        success "salt" ~salt:"this is my salt" [5] "rD";
        success "salt" ~salt:"this is my salt" [12345] "NkK9";
        success "salt" ~salt:"this is my salt" [9007199254740992] "262mm1m4J9Z";
        success "salt" ~salt:"this is my salt" [683; 94108; 123; 5] "aBMswoO2UB3Sj";
        success "salt" ~salt:"Arbitrary string" [683; 94108; 123; 5] "QWyf8yboH7KT2";
        success "salt" ~salt:"Arbitrary string" [1; 2; 3] "neHrCa";
        success "salt" ~salt:"Arbitrary string" [2; 4; 6] "LRCgf2";
        success "salt" ~salt:"Arbitrary string" [99; 25] "JOMh1";
        success "all" ~salt:"this is my salt" ~min_length:0 ~alphabet:"0123456789abcdef" [1234567] "b332db5";
        success "alphabet" ~alphabet:"!\"#%&',-/0123456789:;<=>ABCDEFGHIJKLMNOPQRSTUVWXYZ_`abcdefghijklmnopqrstuvwxyz~" [2839; 12; 32; 5] "_nJUNTVU3";
        success "alphabet" ~alphabet:"!\"#%&',-/0123456789:;<=>ABCDEFGHIJKLMNOPQRSTUVWXYZ_`abcdefghijklmnopqrstuvwxyz~" [1; 2; 3] "7xfYh2";
        success "alphabet" ~alphabet:"!\"#%&',-/0123456789:;<=>ABCDEFGHIJKLMNOPQRSTUVWXYZ_`abcdefghijklmnopqrstuvwxyz~" [23832] "Z6R>";
        success "alphabet" ~alphabet:"!\"#%&',-/0123456789:;<=>ABCDEFGHIJKLMNOPQRSTUVWXYZ_`abcdefghijklmnopqrstuvwxyz~" [99; 25] "AYyIB";
        success "min_length" ~min_length:25 [] "";
        success "min_length" ~min_length:25 [0] "r9JOyLkQWjnegYbwZ1p0GDXNm";
        success "min_length" ~min_length:25 [1] "kL39J4q2VolejRejNmGQBW71g";
        success "min_length" ~min_length:25 [2] "2Pr1gO3GWpmbk5ezJn4KRjLMv";
        success "min_length" ~min_length:25 [7452; 2967; 21401] "pO3K69b86jzc6krI416enr2B5";
        success "min_length" ~min_length:25 [1; 2; 3] "gyOwl4B97bo2fXhVaDR0Znjrq";
        success "min_length" ~min_length:25 [6097] "Nz7x3VXyMYerRmWeOBQn6LlRG";
        success "min_length" ~min_length:25 [99; 25] "k91nqP3RBe3lKfDaLJrvy8XjV";
        success "all" ~salt:"arbitrary salt" ~min_length:16 ~alphabet:"abcdefghijklmnopqrstuvwxyz" [7452; 2967; 21401] "wygqxeunkatjgkrw";
        success "all" ~salt:"arbitrary salt" ~min_length:16 ~alphabet:"abcdefghijklmnopqrstuvwxyz" [1; 2; 3] "pnovxlaxuriowydb";
        success "all" ~salt:"arbitrary salt" ~min_length:16 ~alphabet:"abcdefghijklmnopqrstuvwxyz" [60125] "jkbgxljrjxmlaonp";
        success "all" ~salt:"arbitrary salt" ~min_length:16 ~alphabet:"abcdefghijklmnopqrstuvwxyz" [99; 25] "erdjpwrgouoxlvbx";
        success "alphabet without standard separators" ~alphabet:"abdegjklmnopqrvwxyzABDEGJKLMNOPQRVWXYZ1234567890" [7452; 2967; 21401] "X50Yg6VPoAO4";
        success "alphabet without standard separators" ~alphabet:"abdegjklmnopqrvwxyzABDEGJKLMNOPQRVWXYZ1234567890" [1; 2; 3] "GAbDdR";
        success "alphabet without standard separators" ~alphabet:"abdegjklmnopqrvwxyzABDEGJKLMNOPQRVWXYZ1234567890" [60125] "5NMPD";
        success "alphabet without standard separators" ~alphabet:"abdegjklmnopqrvwxyzABDEGJKLMNOPQRVWXYZ1234567890" [99; 25] "yGya5";
        success "alphabet without standard separators plus salt" ~salt:"salt" ~alphabet:"abdegjklmnopqrvwxyzABDEGJKLMNOPQRVWXYZ1234567890" [99; 25] "zZRnx";
        success "alphabet with standard separators" ~alphabet:"abdegjklmnopqrvwxyzABDEGJKLMNOPQRVWXYZ1234567890uC" [7452; 2967; 21401] "GJNNmKYzbPBw";
        success "alphabet with standard separators" ~alphabet:"abdegjklmnopqrvwxyzABDEGJKLMNOPQRVWXYZ1234567890uC" [1; 2; 3] "DQCXa4";
        success "alphabet with standard separators" ~alphabet:"abdegjklmnopqrvwxyzABDEGJKLMNOPQRVWXYZ1234567890uC" [60125] "38V1D";
        success "alphabet with standard separators" ~alphabet:"abdegjklmnopqrvwxyzABDEGJKLMNOPQRVWXYZ1234567890uC" [99; 25] "373az";
      ]
    );
  ];
]

let () = run_test_tt_main test
