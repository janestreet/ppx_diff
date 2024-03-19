open Base

(* Diff a type that looks like [v ?([@diff.xxx]) Map_module.t] *)
let create
  (kind : How_to_diff.t Type_kind.core_kind)
  ~key
  ~context
  ~(create_core : Core_diff.Create.t)
  =
  let { Context.builder; stable_version; _ } = context in
  let open (val builder : Builder.S) in
  (* [value] = v ?[@diff.xxx]
     [map_module_name] = Map_module
  *)
  let how_to_diff = How_to_diff.Custom.As_map { key } in
  let default_key, value =
    match kind with
    | Constr { params = [ value ]; module_; type_name } ->
      let key = Map_and_set_helper.key_or_elt_heuristic ~module_ ~type_name `Map in
      key, value
    | Constr { params = [ key; value; _comparator_witness ]; module_ = _; type_name = _ }
      ->
      let key = key |> Type_kind.map_core ~f:(fun _ -> ()) |> fst in
      Some key, value
    | _ ->
      raise_error
        (Printf.sprintf
           "\"%s\" diff is only supported for named types with one or three parameters"
           (How_to_diff.Custom.to_string how_to_diff))
  in
  let value_diff = create_core value in
  let { Core_diff.diff_type = value_diff_type
      ; functions =
          { get = get_value_diff
          ; apply_exn = apply_value_diff
          ; of_list_exn = of_list_value_diff
          }
      }
    =
    value_diff
  in
  let module_ =
    let prefix = [ "Diffable"; "Map_diff" ] in
    let suffix =
      match stable_version with
      | None -> []
      | Some One -> [ "Stable"; "V1" ]
    in
    List.map ~f:Module_name.of_string (prefix @ suffix) |> Longident_helper.of_simple_list
  in
  let diff_type =
    (* [Map_module_name].Key.t *)
    let key =
      match key with
      | Some key ->
        Type_kind.core_of_ppx key ~builder |> Type_kind.map_core ~f:(fun _ -> ()) |> fst
      | None ->
        (match default_key with
         | Some key -> key
         | None ->
           raise_error
             "Could not determine key type for map diff. Please provide it manually")
    in
    (* ([Map_module_name].Key.t, [value], [diff_of_value]) Diffable.Map_diff.t *)
    Type_kind.Constr
      { params =
          [ key, (); value |> Type_kind.map_core ~f:(fun _ -> ()); value_diff_type, () ]
      ; module_
      ; type_name = Type_name.t
      }
  in
  let module_ = Option.map module_ ~f:(Longident_helper.map ~f:Module_name.to_string) in
  let fn name =
    Longident_helper.add_suffix module_ ~suffix:(Function_name.to_string name, [])
    |> Longident_helper.to_expression ~builder
  in
  let get = [%expr [%e fn Function_name.get] [%e get_value_diff]] in
  let apply_exn = [%expr [%e fn Function_name.apply_exn] [%e apply_value_diff]] in
  let of_list_exn =
    [%expr
      [%e fn Function_name.of_list_exn] [%e of_list_value_diff] [%e apply_value_diff]]
  in
  { Core_diff.diff_type; functions = { get; apply_exn; of_list_exn } }
;;
