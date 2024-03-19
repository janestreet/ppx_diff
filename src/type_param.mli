open! Base
open! Ppxlib

type t

val of_type_declaration : type_declaration -> builder:Builder.t -> t list

val to_type_declaration_param
  :  t
  -> builder:Builder.t
  -> core_type * (variance * injectivity)

val map_var : t -> f:(Var.t -> Var.t) -> t
val var : t -> Var.t
