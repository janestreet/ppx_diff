open! Base

type t =
  { builder : Builder.t
  ; what_to_derive : What_to_derive.t
  ; all_params : Type_param.t list
  ; sig_or_struct : [ `sig_ | `struct_ ]
  ; stable_version : Stable_version.t option
  }
