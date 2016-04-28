let {Hashids.encode; decode} = Hashids.make ()

let () =
  [4; 8; 15; 16; 23; 42]
  |> encode
  |> print_endline
