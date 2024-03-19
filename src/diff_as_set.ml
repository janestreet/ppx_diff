open Base

let create kind ~elt ~context =
  let { Context.builder; stable_version; _ } = context in
  let open (val builder : Builder.S) in
  let how_to_diff = How_to_diff.Custom.As_set { elt } in
  let default_elt =
    match (kind : _ Type_kind.core_kind) with
    | Constr { params = []; module_; type_name } ->
      Map_and_set_helper.key_or_elt_heuristic ~module_ ~type_name `Set
    | Constr { params = [ elt; _comparator_witness ]; module_ = _; type_name = _ } ->
      Type_kind.map_core elt ~f:(fun _ -> ()) |> fst |> Option.return
    | _ ->
      raise_error
        (Printf.sprintf
           "\"%s\" diff is only supported for named types with no or two parameters"
           (How_to_diff.Custom.to_string how_to_diff))
  in
  (* [Set_module_name].Elt.t *)
  let elt =
    match elt with
    | Some elt ->
      Type_kind.core_of_ppx elt ~builder |> Type_kind.map_core ~f:(fun _ -> ()) |> fst
    | None ->
      (match default_elt with
       | Some elt -> elt
       | None ->
         raise_error
           "Could not determine elt type for set diff. Please provide it manually")
  in
  let module_ =
    let prefix = [ "Diffable"; "Set_diff" ] in
    let suffix =
      match stable_version with
      | None -> []
      | Some One -> [ "Stable"; "V1" ]
    in
    List.map ~f:Module_name.of_string (prefix @ suffix) |> Longident_helper.of_simple_list
  in
  (* [Set_module_name].Elt.t Diffable.Set_diff.t *)
  let diff_type =
    Type_kind.Constr { params = [ elt, () ]; module_; type_name = Type_name.t }
  in
  let module_ = Option.map module_ ~f:(Longident_helper.map ~f:Module_name.to_string) in
  let fn name =
    Longident_helper.add_suffix module_ ~suffix:(Function_name.to_string name, [])
    |> Longident_helper.to_expression ~builder
  in
  { Core_diff.diff_type
  ; functions =
      { get = fn Function_name.get
      ; apply_exn = fn Function_name.apply_exn
      ; of_list_exn = fn Function_name.of_list_exn
      }
  }
;;
