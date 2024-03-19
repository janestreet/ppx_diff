open! Base

val create_core
  :  How_to_diff.t Type_kind.core_kind
  -> atomic:How_to_diff.Atomic.t
  -> sig_or_struct:[ `sig_ | `struct_ ]
  -> builder:Builder.t
  -> Core_diff.t

val create
  :  type_to_diff_declaration:How_to_diff.t Type_declaration.t
  -> atomic:How_to_diff.Atomic.t
  -> sig_or_struct:[ `sig_ | `struct_ ]
  -> builder:Builder.t
  -> Diff.t
