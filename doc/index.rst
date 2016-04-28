=============
hashids-ocaml
=============

.. include:: ../README.rst

Reference
=========

Everything is in the ``Hashids`` module.

.. @todo (Write?) Use a Sphinx domain for OCaml

.. describe:: type t = {encode: int list -> string; decode: string -> int list}

    The type returned by :ref:`make`, containing an encoding and a decoding function.

.. describe:: val make: ?salt:string -> ?min_length:int -> ?alphabet:string -> unit -> t

    ``salt``: an arbitrary string used to randomize the encoding.

    ``min_length``: an integer specifying the minimal length of encoded values.
    Useful to hide their order of magnitude.

    ``alphabet``: the set of characters used to encode.
