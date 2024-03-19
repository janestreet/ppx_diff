open Base
open Ppxlib

module Flags = struct
  type t =
    { private_ : bool
    ; nonrec_ : bool
    }

  let empty = { private_ = false; nonrec_ = false }
end

type 'extra t =
  { params : Type_param.t list
  ; name : Type_name.t
  ; kind : 'extra Type_kind.t
  ; unboxed : bool
  }

let map t ~f = { t with kind = Type_kind.map t.kind ~f }

let create td ~builder ~how_to_diff =
  let kind, how_to_diff =
    Type_kind.of_ppx_kind (td.ptype_kind, td.ptype_manifest) ~builder ~how_to_diff
  in
  ( { params = Type_param.of_type_declaration td ~builder
    ; name = Type_name.of_string td.ptype_name.txt
    ; kind
    ; unboxed = Unboxed.is_unboxed td
    }
  , how_to_diff )
;;

let to_items ?(flags = Flags.empty) (t : unit t) ~context =
  let { Context.builder
      ; what_to_derive
      ; all_params = _
      ; sig_or_struct = _
      ; stable_version = _
      }
    =
    context
  in
  let open (val builder : Builder.S) in
  let { kind; name; params; unboxed } = t in
  let { Flags.private_; nonrec_ } = flags in
  let rec_flag = if nonrec_ then Nonrecursive else Recursive in
  let type_sig t = psig_type rec_flag [ t ] in
  let type_str t = pstr_type rec_flag [ t ] in
  let ptype_name = Located.mk (Type_name.to_string name) in
  let ptype_params = List.map params ~f:(Type_param.to_type_declaration_param ~builder) in
  let ptype_attributes ~unboxed =
    List.filter_opt
      [ What_to_derive.attribute what_to_derive ~builder
      ; (if unboxed then Some (Unboxed.attribute ~builder) else None)
      ]
  in
  let td ~private_ ~simplify =
    let kind, simplified =
      if not simplify
      then kind, false
      else (
        match kind with
        | Core _ | Abstract
        | Record { equal_to = None; _ }
        | Variant { equal_to = None; _ } -> kind, false
        | Record { equal_to = Some kind; _ } | Variant { equal_to = Some kind; _ } ->
          Core kind, true)
    in
    let ptype_kind, ptype_manifest = Type_kind.to_ppx_kind kind ~builder in
    { ptype_name
    ; ptype_params
    ; ptype_cstrs = []
    ; ptype_kind
    ; ptype_private = (if private_ then Private else Public)
    ; ptype_manifest
    ; ptype_attributes = ptype_attributes ~unboxed:(unboxed && not simplified)
    ; ptype_loc = loc
    }
  in
  { Items.sig_items = [ type_sig (td ~private_ ~simplify:true) ]
  ; struct_items = Ok [ type_str (td ~private_:false ~simplify:false) ]
  }
;;

let pointer ?(module_ = []) { params; name; kind = _; unboxed = _ } =
  ( Type_kind.Constr
      { params =
          List.map params ~f:(fun param -> Type_kind.Var (Type_param.var param), ())
      ; module_ = Longident_helper.of_simple_list module_
      ; type_name = name
      }
  , () )
;;
