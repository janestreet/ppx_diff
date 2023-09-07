open! Core
open Ppxlib

val create
  :  How_to_diff.t Type_kind.core_kind
  -> elt:core_type option
  -> builder:Builder.t
  -> Core_diff.t
