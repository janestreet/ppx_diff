open! Base
open Ppxlib

module Type_ : sig
  type t

  val pattern : (expression, t -> t option, t option) Ast_pattern.t
end

module Label : sig
  val how : string
  val key : string
  val elt : string
end

module Atomic : sig
  type t = { using_compare : bool }

  val to_string : t -> string
end

module Custom : sig
  type t =
    | Atomic of Atomic.t
    | As_set of { elt : core_type option }
    | As_map of { key : core_type option }

  val of_core_type : core_type -> builder:Builder.t -> t option
  val of_label_declaration : label_declaration -> builder:Builder.t -> t option

  val of_constructor_declaration
    :  constructor_declaration
    -> builder:Builder.t
    -> t option

  val of_rtag : row_field -> builder:Builder.t -> t option
  val to_string : t -> string
  val to_attribute_string : t -> string

  module Or_abstract : sig
    type nonrec t =
      | Custom of t
      | Abstract

    val to_string : t -> string
  end
end

module Maybe_abstract : sig
  type t = Custom.Or_abstract.t option

  val create
    :  how:string option
    -> key:Type_.t option
    -> elt:Type_.t option
    -> builder:Builder.t
    -> t
end

type t = Custom.t option
