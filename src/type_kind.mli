open! Base
open Ppxlib

type 'extra core_kind =
  | Var of Var.t
  | Tuple of 'extra core list
  | Constr of 'extra constr
  | Polymorphic_variant of 'extra polymorphic_variant

and 'extra core = 'extra core_kind * 'extra

and 'extra constr =
  { params : 'extra core list
  ; module_ : Module_name.t Longident_helper.t option
  ; type_name : Type_name.t
  }

and 'extra polymorphic_variant = (Variant_row_name.t * 'extra core option) list

type 'extra record_field =
  { field_name : Record_field_name.t
  ; field_type : 'extra core
  ; mutable_ : bool
  ; global : bool
  }

type 'extra variant_row_type =
  | Single of 'extra core
  | Inlined_record of 'extra record_field list
  | Inlined_tuple of 'extra core list

type 'extra variant = (Variant_row_name.t * 'extra variant_row_type option) list

type 'extra t =
  | Abstract
  | Core of 'extra core
  | Variant of
      { equal_to : 'extra core option
      ; rows : 'extra variant
      }
  | Record of
      { equal_to : 'extra core option
      ; fields : 'extra record_field list
      ; local : bool
      }

(* In a type declaration like [ type t = foo = { bar : int } ],
   The core_type is [foo] and the type_kind is [{bar : int}]. *)
type ppx_kind := type_kind * core_type option

val of_ppx_kind
  :  ppx_kind
  -> how_to_diff:How_to_diff.Maybe_abstract.t
  -> builder:Builder.t
  -> How_to_diff.t t * How_to_diff.Atomic.t option

val to_ppx_kind : unit t -> builder:Builder.t -> ppx_kind
val core_to_ppx : unit core -> builder:Builder.t -> Ppxlib.core_type
val core_of_ppx : core_type -> builder:Builder.t -> How_to_diff.t core
val vars : _ t -> Var.t list
val constrs : 'a t -> 'a constr list
val is_local : _ t -> bool
val map : 'a t -> f:('a -> 'b) -> 'b t
val map_core : 'a core -> f:('a -> 'b) -> 'b core
val map_variant_row_type : 'a variant_row_type -> f:('a -> 'b) -> 'b variant_row_type
val can_be_unboxed : _ t -> bool
