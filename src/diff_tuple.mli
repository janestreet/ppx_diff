open! Base

val create
  :  ?inlined:bool
  -> How_to_diff.t Type_kind.core list
  -> builder:Builder.t
  -> create_core:Core_diff.Create.t
  -> Core_diff.t
