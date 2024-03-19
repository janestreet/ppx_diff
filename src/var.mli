(* Represents a type variable, e.g. ['a] in ['a t] *)
open! Base
open! Ppxlib

type t

include Comparable.S with type t := t
include Stringable.S with type t := t

val core_type : t -> builder:Builder.t -> core_type
val diff_var : t -> t
