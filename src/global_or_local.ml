open Core
open Ppxlib

type t =
  | Global
  | Local

let to_string = function
  | Global -> "global"
  | Local -> "local"
;;

let attribute t ~builder =
  let open (val builder : Builder.S) in
  attribute ~name:(Located.mk ("extension." ^ to_string t)) ~payload:(PStr [])
;;

let add_to_core_type t core_type ~builder =
  { core_type with
    ptyp_attributes = core_type.ptyp_attributes @ [ attribute t ~builder ]
  }
;;
