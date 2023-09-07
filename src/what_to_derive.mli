open! Core
open! Ppxlib

module Entry : sig
  type t

  val sexp_of : t
  val of_sexp : t
  val sexp : t
  val bin_io : t
  val compare : t
  val equal : t
  val variants : t
end

type t

val empty : t
val create : type_declaration -> How_to_diff.Atomic.t option -> [ `sig_ | `struct_ ] -> t
val add : t -> Entry.t -> t
val mem : t -> Entry.t -> bool
val attribute : t -> builder:Builder.t -> attribute option
