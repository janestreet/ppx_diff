open Base
open Ppxlib

type t =
  { sig_items : signature_item list
  ; struct_items : structure_item list Or_error.t
  }

let empty = { sig_items = []; struct_items = Ok [] }

let concat l =
  { sig_items = List.concat (List.map l ~f:(fun t -> t.sig_items))
  ; struct_items =
      List.map l ~f:(fun t -> t.struct_items)
      |> Or_error.combine_errors
      |> Or_error.map ~f:List.concat
  }
;;

let to_module (t : t) ~module_name ~builder : t =
  let open (val builder : Builder.S) in
  let { sig_items; struct_items } = t in
  let module_name = module_name |> Module_name.to_string |> Option.return |> Located.mk in
  let module_type = sig_items |> pmty_signature in
  let struct_items =
    Result.map struct_items ~f:(fun struct_items ->
      let module_expr = pmod_structure struct_items in
      [ module_binding ~name:module_name ~expr:module_expr |> pstr_module ])
  in
  let sig_items =
    [ module_declaration ~name:module_name ~type_:module_type |> psig_module ]
  in
  { sig_items; struct_items }
;;
