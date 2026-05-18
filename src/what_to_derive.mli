open! Base
open! Ppxlib

module Entry : sig
  type t

  val sexp_of : t
  val of_sexp : t
  val sexp : t
  val bin_io : t
  val compare_ : t
  val equal_ : t
  val variants : t
  val to_string : t -> string
end

module Extra : sig
  type t

  val arg : t option Deriving.Args.param
end

type t

val empty : t

val create
  :  ?extra:Extra.t
  -> type_declaration
  -> How_to_diff.Atomic.t option
  -> [ `sig_ | `struct_ ]
  -> builder:Builder.t
  -> t

val add : t -> Entry.t -> t
val mem : t -> Entry.t -> bool
val attribute : t -> builder:Builder.t -> attribute option
