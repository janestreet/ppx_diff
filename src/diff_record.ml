open Core
open Ppxlib
open Build_helper

(* The diff type of a record follows the same pattern as the diff type of a tuple

   The diff for

   type 'a t = { x : X.t ; y : Y.t ; ... }

   will be:

   {[type ('a, 'a_diff) single =
       | X of X.Diff.t
       | Y of Y.Diff.t
       (* etc *)

     type ('a, 'a_diff) t = private ('a, 'a_diff) single list
   ]}

   The entries in the list are guaranteed to be sorted and unique (i.e at most one per field)

   There is a special case for a diff with one field: we don't return a list, just the one
   single element.
*)

let variant_row_name ~field_name =
  field_name
  |> Record_field_name.to_string
  |> String.capitalize
  |> Variant_row_name.of_string
;;

let variant_row ~field_name prefix =
  Variant_row
    { name = variant_row_name ~field_name
    ; polymorphic = false
    ; value = Some (Text prefix)
    }
;;

let with_prefix ~field_name prefix : Build_helper.t =
  Text (Prefix.to_prefix prefix ^ Record_field_name.to_string field_name)
;;

let record ~field_names prefix =
  Record
    { module_ = None
    ; fields =
        List.map field_names ~f:(fun field_name ->
          { field_name; field_value = with_prefix ~field_name prefix })
    }
;;

module Field_diff = struct
  type t =
    { field_name : Record_field_name.t
    ; field_diff : Core_diff.t
    }
end

module Field_diffs = struct
  type t =
    | Single of Field_diff.t
    | Multi of Field_diff.t list

  let to_list = function
    | Single field_diff -> [ field_diff ]
    | Multi field_diffs -> field_diffs
  ;;
end

let diff_type_kind ~field_diffs =
  let open Diff.Diff_type_kind in
  let variant_row { Field_diff.field_name; field_diff } =
    variant_row_name ~field_name, (field_diff.diff_type, ())
  in
  match (field_diffs : Field_diffs.t) with
  | Single field_diff ->
    This
      { kind =
          Variant
            { rows =
                [ Tuple2.map_snd (variant_row field_diff) ~f:(fun type_ ->
                    Some (Type_kind.Single type_))
                ]
            ; equal_to = None
            }
      ; nonrec_ = false
      ; unboxed_override = None
      }
  | Multi field_diffs ->
    let single_kind = List.map field_diffs ~f:variant_row in
    Sorted_list { single_kind; single_module_name = Module_name.of_string "Field_diff" }
;;

let get ~field_diffs ~builder =
  let field_names =
    List.map (Field_diffs.to_list field_diffs) ~f:(fun diff -> diff.Field_diff.field_name)
  in
  let open (val builder : Builder.S) in
  let diff_of_field ~field_name =
    let txt = with_prefix ~field_name in
    [%expr
      [%e txt (Some Prefix.get) |> e]
        ~from:[%e txt (Some Prefix.from) |> e]
        ~to_:[%e txt (Some Prefix.to_) |> e]]
  in
  List.fold
    (Field_diffs.to_list field_diffs)
    ~f:(fun expr { field_name; field_diff } ->
      (* Pre-allocate the functions to get specific entry diffs. Without this, diffs
         for parametrized types would allocate the closures. Tested by
         [test_record.ml:parametrized], which allocates without this *)
      [%expr
        let [%p with_prefix ~field_name (Some Prefix.get) |> p] =
          [%e field_diff.functions.get]
        in
        [%e expr]])
    ~init:
      [%expr
        fun ~from ~to_ ->
          if Base.phys_equal from to_
          then Optional_diff.none
          else (
            (* let { x = from_x ; y = from_y ; ... } in
               let { x = to_x; y = to_y ; ... } in
               let diff = [] in
            *)
            let [%p record ~field_names (Some Prefix.from) |> p] = from in
            let [%p record ~field_names (Some Prefix.to_) |> p] = to_ in
            [%e
              match field_diffs with
              | Single { Field_diff.field_name; field_diff = _ } ->
                [%expr
                  Optional_diff.map [%e diff_of_field ~field_name] ~f:(fun d ->
                    [%e variant_row ~field_name "d" |> e])]
              | Multi field_diffs ->
                [%expr
                  let diff = [] in
                  [%e
                    List.fold
                      field_diffs
                      ~f:(fun expr { Field_diff.field_name; field_diff = _ } ->
                        (*
                           {[let diff =
                               let d = Y.Diff.get ~from:from_y ~to_:to_y in
                               if Optional_diff.is_none d
                               then diff
                               else
                                 let d = Optional_diff.unsafe_value d in
                                 Y d :: diff
                             in
                             (*... *)
                           ]}

                           (Note that this has to go in reverse order of record fields , so that we
                           don't need to allocate a rev list at the end) *)
                        [%expr
                          let diff =
                            let d = [%e diff_of_field ~field_name] in
                            if Optional_diff.is_none d
                            then diff
                            else (
                              let d = Optional_diff.unsafe_value d in
                              [%e variant_row ~field_name "d" |> e] :: diff)
                          in
                          [%e expr]])
                      ~init:
                        [%expr
                          match diff with
                          | [] -> Optional_diff.none
                          | _ :: _ -> Optional_diff.return diff]]]])]
;;

let apply ~field_diffs ~local_apply ~builder =
  let field_names =
    List.map (Field_diffs.to_list field_diffs) ~f:(fun diff -> diff.Field_diff.field_name)
  in
  let open (val builder : Builder.S) in
  let return_expr expr = if local_apply then [%expr [%e expr]] else expr in
  let apply_field_diff ~field_name d =
    let txt = with_prefix ~field_name in
    [%expr
      [%e txt (Some Prefix.apply_exn) |> e] [%e txt (Some Prefix.derived_on) |> e] [%e d]]
  in
  List.fold
    (Field_diffs.to_list field_diffs)
    ~f:(fun expr { field_name; field_diff } ->
      (* Pre-allocate the [apply] functions *)
      [%expr
        let [%p with_prefix ~field_name (Some Prefix.apply_exn) |> p] =
          [%e field_diff.functions.apply_exn]
        in
        [%e expr]])
    ~init:
      [%expr
        fun derived_on diff ->
          (* {[ let { x = derived_on_x ; y = derived_on_y ; ... } = derived_on ]} *)
          let [%p record ~field_names (Some Prefix.derived_on) |> p] = derived_on in
          [%e
            match field_diffs with
            | Single { field_name; field_diff = _ } ->
              let txt = with_prefix ~field_name in
              [%expr
                let [%p txt None |> p] =
                  let [%p variant_row ~field_name "d" |> p] = diff in
                  [%e apply_field_diff ~field_name (Text "d" |> e)]
                in
                [%e return_expr (record ~field_names None |> e)]]
            | Multi field_diffs ->
              List.fold_right
                field_diffs
                ~f:(fun { field_name; field_diff = _ } expr ->
                  let txt = with_prefix ~field_name in
                  (* {[ let x, diff =
                          match diff with
                          | T1 d :: tl -> X.Diff.apply_exn derived_on1 d, tl
                          | _ -> derived_on1, diff
                       in
                       (* ... *)
                     ]}
                  *)
                  [%expr
                    let [%p txt None |> p], diff =
                      match diff with
                      | [%p variant_row ~field_name "d" |> p] :: tl ->
                        [%e apply_field_diff ~field_name (Text "d" |> e)], tl
                      | _ -> [%e txt (Some Prefix.derived_on) |> e], diff
                    in
                    [%e expr]])
                ~init:
                  (* {[ match diff with
                       | [] -> { x ; y ; ... }
                       | _ :: _ -> BUG
                     ]}*)
                  (return_expr
                     [%expr
                       match diff with
                       | [] -> [%e record ~field_names None |> e]
                       | _ :: _ -> failwith "BUG: non-empty diff after apply"])]]
;;

let create ~field_diffs ~local_apply ~builder =
  let field_diffs =
    match field_diffs with
    | [ diff ] -> Field_diffs.Single diff
    | diffs -> Multi diffs
  in
  let get = get ~field_diffs ~builder in
  let apply_exn = apply ~field_diffs ~local_apply ~builder in
  { Diff.prefix = Items.empty
  ; diff_type = diff_type_kind ~field_diffs
  ; functions = Ok { get; apply_exn }
  }
;;

let create_record
  ~(record_fields : How_to_diff.t Type_kind.record_field list)
  ~builder
  ~(create_core : Core_diff.Create.t)
  ~local
  =
  let field_diffs =
    List.map record_fields ~f:(fun { field_name; field_type; mutable_ = _; global = _ } ->
      let field_diff = create_core field_type in
      { Field_diff.field_name; field_diff })
  in
  create ~field_diffs ~builder ~local_apply:local
;;
