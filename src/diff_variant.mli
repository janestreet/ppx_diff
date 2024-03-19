open! Base

val create
  :  How_to_diff.t Type_kind.variant
  -> context:Context.t
  -> create_core:Core_diff.Create.t
  -> Diff.t

val create_polymorphic
  :  How_to_diff.t Type_kind.polymorphic_variant
  -> context:Context.t
  -> create_core:Core_diff.Create.t
  -> Core_diff.t
