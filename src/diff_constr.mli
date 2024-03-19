open! Base

val create
  :  How_to_diff.t Type_kind.constr
  -> create_core:Core_diff.Create.t
  -> builder:Builder.t
  -> Core_diff.t
