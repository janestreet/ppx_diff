open! Base
open Ppxlib
open Shared

module type Ast_builders = sig
  include Ast_builder.S
  include Ppxlib_jane.Ast_builder.S_with_implicit_loc
end

module type S = sig
  include Ast_builders

  val p : Build_helper.t -> pattern
  val e : Build_helper.t -> expression
  val raise_error : string -> 'a
end

type t = (module S)

let create (module M : Ast_builders) =
  let module M : S = struct
    include M
    open Build_helper

    let record ~module_ ~fields ~f =
      List.mapi fields ~f:(fun i { field_name; field_value } ->
        ( Located.mk
            (match i, module_ with
             | 0, Some module_ ->
               Ldot
                 ( Lident (Module_name.to_string module_)
                 , Record_field_name.to_string field_name )
             | _ -> Lident (Record_field_name.to_string field_name))
        , f field_value ))
    ;;

    let v { name; polymorphic; value } ~f_value ~f_variant ~f_construct =
      let name = Variant_row_name.to_string name in
      let row_value = Option.map value ~f:f_value in
      if polymorphic
      then f_variant name row_value
      else f_construct (Located.mk (Lident name)) row_value
    ;;

    let rec pattern t =
      match t with
      | Text "_" -> ppat_any
      | Text v -> ppat_var (Located.mk v)
      | Tuple l -> ppat_tuple (List.map l ~f:pattern)
      | Record { module_; fields } ->
        ppat_record (record ~module_ ~fields ~f:pattern) Closed
      | Variant_row row ->
        v row ~f_value:pattern ~f_variant:ppat_variant ~f_construct:ppat_construct
      | Local_expr t -> pattern t
    ;;

    let rec expression t =
      match t with
      | Text s -> pexp_ident (Located.mk (Lident s))
      | Tuple l -> pexp_tuple (List.map l ~f:expression)
      | Record { module_; fields } ->
        pexp_record (record ~module_ ~fields ~f:expression) None
      | Variant_row row ->
        v row ~f_value:expression ~f_variant:pexp_variant ~f_construct:pexp_construct
      | Local_expr t -> [%expr [%e expression t]]
    ;;

    let p = pattern
    let e = expression
    let raise_error s = Location.raise_errorf ~loc "ppx_%s: %s" name_of_ppx s
  end
  in
  (module M : S)
;;
