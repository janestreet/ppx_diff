open! Base
open Ppxlib

(** Wrapper around [Longident] that's easier to work with.

    e.g.
    "t" is represented as [Simple [ "t"]]
    "X.Diff.t" is represented as [Simple [ "X"; "Diff"; "t"]]
    "A.B(C).D.t" is represented as
    [Functor_application (Simple ["A"; "B"], Simple ["C"], [ "D"; "t" ]]
*)

type 'a t =
  | Simple of 'a Nonempty_list.t
  | Functor_application of 'a t * 'a t * 'a list

val of_longident : Longident.t -> builder:Builder.t -> string t
val to_longident : string t -> Longident.t
val to_expression : string t -> builder:Builder.t -> expression
val map : 'a t -> f:('a -> 'b) -> 'b t
val of_simple_list : 'a list -> 'a t option

val to_simple_list
  :  'a t option
  -> on_functor_application:('a t * 'a t -> Error.t)
  -> builder:Builder.t
  -> 'a list

val add_suffix : 'a t option -> suffix:'a Nonempty_list.t -> 'a t
