open! Base

val create_record
  :  record_fields:How_to_diff.t Type_kind.record_field list
  -> builder:Builder.t
  -> create_core:Core_diff.Create.t
  -> local:bool
  -> Diff.t
