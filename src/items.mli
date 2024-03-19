(* Helper module that allows us to keep [signature_item list] and [structure_item list]
   together.  That way we can compose them easier:

   If we know that each of [t1], [t2], ... are well-formed, i.e. [struct_items] implement
   [sig_items], the same should be true for [concat [t1 ; t2 ; ...]]
*)

open! Base
open Ppxlib

type t =
  { sig_items : signature_item list
  ; struct_items : structure_item list Or_error.t
  }

val empty : t
val concat : t list -> t
val to_module : t -> module_name:Module_name.t -> builder:Builder.t -> t
