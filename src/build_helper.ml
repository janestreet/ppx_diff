open! Base

type t =
  | Text of string
  | Tuple of t list
  | Variant_row of variant_row
  | Record of
      { module_ : Module_name.t option
      ; fields : record_field list
      }
  | Local_expr of t

and variant_row =
  { name : Variant_row_name.t
  ; polymorphic : bool
  ; value : t option
  }

and record_field =
  { field_name : Record_field_name.t
  ; field_value : t
  }
