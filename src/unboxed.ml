open Base
open Ppxlib

let attribute_name = "unboxed"

let is_unboxed (td : type_declaration) =
  List.exists td.ptype_attributes ~f:(fun { attr_name; _ } ->
    String.( = ) attr_name.txt attribute_name)
;;

let attribute ~builder =
  let open (val builder : Builder.S) in
  attribute ~name:(Located.mk attribute_name) ~payload:(PStr [])
;;
