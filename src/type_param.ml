open! Base
open Ppxlib

type t =
  { var : Var.t
  ; variance : variance
  ; injectivity : injectivity
  }

let map_var t ~f = { t with var = f t.var }
let var t = t.var

let of_type_declaration td ~builder =
  let open (val builder : Builder.S) in
  List.map td.ptype_params ~f:(function
    | { ptyp_desc = Ptyp_var var; _ }, (variance, injectivity) ->
      { var = Var.of_string var; variance; injectivity }
    | _ -> raise_error "Unexpected type param, expected vars only")
;;

let to_type_declaration_param t ~builder =
  let { var; variance; injectivity } = t in
  Var.core_type var ~builder, (variance, injectivity)
;;
