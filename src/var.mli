(* Represents a type variable, e.g. ['a] in ['a t] *)
open! Core
open! Ppxlib

type t

include Comparable with type t := t
include Stringable with type t := t

val core_type : t -> builder:Builder.t -> core_type
val diff_var : t -> t
