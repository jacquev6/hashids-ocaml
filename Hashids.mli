(* Copyright 2016-2017 Vincent Jacques <vincent@vincent-jacques.net> *)

(** The type returned by :val:`.make`, containing an encoding and a decoding function. *)
type t = {
  encode: int list -> string;
  decode: string -> int list;
}

val make: ?salt:string -> ?min_length:int -> ?alphabet:string -> unit -> t
(**
    ``salt``: an arbitrary string used to randomize the encoding.

    ``min_length``: an integer specifying the minimal length of encoded values.
    Useful to hide their order of magnitude.

    ``alphabet``: the set of characters used to encode.
*)
