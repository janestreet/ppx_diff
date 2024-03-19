open! Base
open Ppxlib

val create
  :  How_to_diff.t Type_kind.core_kind
  -> key:core_type option
  -> context:Context.t
  -> create_core:Core_diff.Create.t
  -> Core_diff.t
