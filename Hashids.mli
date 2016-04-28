(* Copyright 2016 Vincent Jacques <vincent@vincent-jacques.net> *)

type t = {
  encode: int list -> string;
  decode: string -> int list;
}

val make: ?salt:string -> ?min_length:int -> ?alphabet:string -> unit -> t
