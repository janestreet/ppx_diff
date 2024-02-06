open Core

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
  let value, map_module_name, map_type_name =
    match kind with
    | Constr { params = [ value ]; module_; type_name } -> value, module_, type_name
    | _ ->
      raise_error
        (sprintf
           "\"%s\" diff is only supported for named types with a single parameter"
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
        Type_kind.core_of_ppx key ~builder |> Type_kind.map_core ~f:(const ()) |> fst
      | None ->
        let map_module_name =
          Longident_helper.to_simple_list
            map_module_name
            ~builder
            ~on_functor_application:
              (const
                 (Error.createf
                    "if using functor application with \"%s\" diff, you need to specify \
                     \"%s\""
                    (How_to_diff.Custom.to_string how_to_diff)
                    How_to_diff.Label.key))
        in
        Type_kind.Constr
          { params = []
          ; module_ =
              map_module_name
              @ [ Module_name.generate ~prefix:"Key" ~type_name:map_type_name ]
              |> Longident_helper.of_simple_list
          ; type_name = Type_name.t
          }
    in
    (* ([Map_module_name].Key.t, [value], [diff_of_value]) Diffable.Map_diff.t *)
    Type_kind.Constr
      { params =
          [ key, (); value |> Type_kind.map_core ~f:(const ()); value_diff_type, () ]
      ; module_
      ; type_name = Type_name.t
      }
  in
  let module_ = Option.map module_ ~f:(Longident_helper.map ~f:Module_name.to_string) in
  let fn name =
    Longident_helper.add_suffix module_ ~suffix:[ Function_name.to_string name ]
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
