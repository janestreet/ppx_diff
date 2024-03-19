open Base
open Ppxlib

let error_on_custom_how_to_diff how_to_diff ~atomic ~builder =
  let open (val builder : Builder.S) in
  match how_to_diff with
  | None -> ()
  | Some how_to_diff ->
    raise_error
      (Printf.sprintf
         "%s will be ignored because it is inside a type already marked %s"
         (How_to_diff.Custom.to_attribute_string how_to_diff)
         (How_to_diff.Custom.to_string (Atomic atomic)))
;;

let validate_no_vars vars atomic ~builder =
  match vars with
  | [] -> ()
  | _ :: _ ->
    let open (val builder : Builder.S) in
    raise_error
      (Printf.sprintf
         "[%s] is not supported for parametrized types"
         (How_to_diff.Custom.to_string (Atomic atomic)))
;;

let validate_sig_or_struct ~atomic ~sig_or_struct ~builder =
  match atomic, sig_or_struct with
  | _, `struct_ | { How_to_diff.Atomic.using_compare = false }, `sig_ -> ()
  | { using_compare = true }, `sig_ ->
    let open (val builder : Builder.S) in
    let atomic_using_compare = How_to_diff.Atomic.to_string { using_compare = true } in
    let atomic_using_equal = How_to_diff.Atomic.to_string { using_compare = false } in
    raise_error
      (Printf.sprintf
         "[%s] is not supported in signatures/mlis, please use [%s] instead (which will \
          still work if you use [%s] in the structure/ml)"
         atomic_using_compare
         atomic_using_equal
         atomic_using_compare)
;;

let create_functions kind ~atomic ~sig_or_struct ~builder =
  let open (val builder : Builder.S) in
  validate_no_vars (Type_kind.vars (Core kind)) atomic ~builder;
  validate_sig_or_struct ~atomic ~sig_or_struct ~builder;
  let equal =
    let type_ = Type_kind.core_to_ppx kind ~builder in
    let { How_to_diff.Atomic.using_compare } = atomic in
    if using_compare
    then [%expr [%compare.equal: [%t type_]]]
    else [%expr [%equal: [%t type_]]]
  in
  let get =
    [%expr
      fun ~from ~to_ ->
        if Core.phys_equal from to_ || [%e equal] from to_
        then Optional_diff.none
        else Optional_diff.return to_]
  in
  let apply_exn = [%expr fun _derived_on diff -> diff] in
  let of_list_exn =
    [%expr
      function
      | [] -> Optional_diff.none
      | _ :: _ as l -> Optional_diff.return (Base.List.last_exn l)]
  in
  { Diff.Functions.get; apply_exn; of_list_exn }
;;

let create_core (kind : How_to_diff.t Type_kind.core_kind) ~atomic ~sig_or_struct ~builder
  : Core_diff.t
  =
  let open (val builder : Builder.S) in
  let kind, () =
    Type_kind.map_core (kind, None) ~f:(error_on_custom_how_to_diff ~atomic ~builder)
  in
  { Core_diff.diff_type = kind
  ; functions = create_functions (kind, ()) ~atomic ~sig_or_struct ~builder
  }
;;

let create ~type_to_diff_declaration ~atomic ~sig_or_struct ~builder =
  let { Type_declaration.kind = kind_to_diff; name = _; params; unboxed } =
    type_to_diff_declaration
  in
  let kind_to_diff =
    Type_kind.map kind_to_diff ~f:(error_on_custom_how_to_diff ~atomic ~builder)
  in
  validate_no_vars params atomic ~builder;
  let type_ type_name = Type_kind.Constr { params = []; module_ = None; type_name } in
  let functions =
    create_functions (type_ Type_name.t, ()) ~atomic ~sig_or_struct ~builder
  in
  let kind, nonrec_ =
    let pointer =
      Type_declaration.pointer
        { type_to_diff_declaration with name = Type_name.derived_on }
    in
    match kind_to_diff with
    | Core (Constr { type_name; module_ = None; params = [] }, ()) as kind ->
      kind, Type_name.( = ) type_name Type_name.t
    | Core _ as kind -> kind, false
    | Abstract -> Core pointer, false
    | Record { fields; local; equal_to = _ } ->
      Record { fields; local; equal_to = Some pointer }, false
    | Variant { rows; equal_to = _ } -> Variant { rows; equal_to = Some pointer }, false
  in
  { Diff.prefix = Items.empty
  ; diff_type = This { kind; nonrec_; unboxed_override = Some unboxed }
  ; functions = Ok functions
  }
;;
