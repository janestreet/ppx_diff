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

let validate (atomic : How_to_diff.Atomic.t) ~vars ~builder ~sig_or_struct =
  let to_string atomic = How_to_diff.Custom.to_string (Atomic atomic) in
  match sig_or_struct with
  | `sig_ ->
    (match atomic with
     | Using_equal -> ()
     | Using_equal_via_get | Using_compare ->
       let open (val builder : Builder.S) in
       let not_supported = How_to_diff.Atomic.to_string atomic in
       let supported = How_to_diff.Atomic.to_string Using_equal in
       raise_error
         (Printf.sprintf
            "[%s] is not supported in signatures/mlis, please use [%s] instead (which \
             will still work if you use [%s] in the structure/ml)"
            not_supported
            supported
            not_supported))
  | `struct_ ->
    (match vars, atomic with
     | [], (Using_equal | Using_compare) | _ :: _, Using_equal_via_get -> ()
     | [], Using_equal_via_get ->
       let open (val builder : Builder.S) in
       raise_error
         (Printf.sprintf
            "[%s] can only be used for parametrized types. Please use [%s] or [%s]."
            (to_string Using_equal_via_get)
            (to_string Using_equal)
            (to_string Using_compare))
     | _ :: _, (Using_equal | Using_compare) ->
       let open (val builder : Builder.S) in
       raise_error
         (Printf.sprintf
            "[%s] is not supported for parametrized types. You can use [%s], but it has \
             performance implications, so make sure to read the docs first."
            (How_to_diff.Custom.to_string (Atomic atomic))
            (How_to_diff.Custom.to_string (Atomic Using_equal_via_get))))
;;

let create_functions kind ~atomic ~sig_or_struct ~builder =
  let open (val builder : Builder.S) in
  let vars = Type_kind.vars (Core kind) in
  validate atomic ~vars ~sig_or_struct ~builder;
  let equal =
    let type_ = Type_kind.core_to_ppx kind ~builder in
    match atomic with
    | Using_compare -> [%expr [%compare.equal: [%t type_]]]
    | Using_equal | Using_equal_via_get -> [%expr [%equal: [%t type_]]]
  in
  let get =
    List.fold
      vars
      ~init:
        [%expr
          fun ~from ~to_ -> exclave_
            if Core.phys_equal from to_ || [%e equal] from to_
            then Optional_diff.get_none ()
            else Optional_diff.return to_]
      ~f:(fun expr var ->
        [%expr
          let [%p Build_helper.Text ("_cmp__" ^ Var.to_string var) |> p] =
            fun from to_ ->
            Optional_diff.is_none
              ([%e Function_name.function_of_var Function_name.get var |> e] ~from ~to_)
            [@nontail]
          in
          [%e expr]])
  in
  let apply_exn = [%expr fun _derived_on diff -> diff] in
  let of_list_exn =
    [%expr
      fun l -> exclave_
        match l with
        | [] -> Optional_diff.get_none ()
        | _ :: _ -> Optional_diff.return (Base.List.last_exn l)]
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
  let type_ type_name =
    let params =
      List.map params ~f:(fun param -> Type_kind.Var (Type_param.var param), ())
      @ List.map params ~f:(fun _ -> Type_kind.Any, ())
    in
    Type_kind.Constr { params; module_ = None; type_name }
  in
  let functions =
    create_functions (type_ Type_name.t, ()) ~atomic ~sig_or_struct ~builder
  in
  let kind, nonrec_ =
    let pointer =
      Type_declaration.pointer
        { type_to_diff_declaration with name = Type_name.derived_on }
    in
    match kind_to_diff with
    | Core (Constr { type_name; module_ = None; params = _ }, ()) as kind ->
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
