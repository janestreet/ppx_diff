open Core

let create kind ~elt ~builder =
  let open (val builder : Builder.S) in
  let how_to_diff = How_to_diff.Custom.As_set { elt } in
  let set_module_name, set_type_name =
    match (kind : _ Type_kind.core_kind) with
    | Constr { params = []; module_; type_name } -> module_, type_name
    | _ ->
      raise_error
        (sprintf
           "\"%s\" diff is only supported for named types with no parameters"
           (How_to_diff.Custom.to_string how_to_diff))
  in
  (* [Set_module_name].Elt.t *)
  let elt =
    match elt with
    | Some elt ->
      Type_kind.core_of_ppx elt ~builder |> Type_kind.map_core ~f:(const ()) |> fst
    | None ->
      let set_module_name =
        Longident_helper.to_simple_list
          set_module_name
          ~builder
          ~on_functor_application:
            (const
               (Error.createf
                  "if using functor application with \"%s\" diff, you need to specify \
                   \"%s\""
                  (How_to_diff.Custom.to_string how_to_diff)
                  How_to_diff.Label.elt))
      in
      Type_kind.Constr
        { params = []
        ; module_ =
            set_module_name
            @ [ Module_name.generate ~prefix:"Elt" ~type_name:set_type_name ]
            |> Longident_helper.of_simple_list
        ; type_name = Type_name.t
        }
  in
  (* [Set_module_name].Elt.t Ldiffable.Set_diff.t *)
  let diff_type =
    Type_kind.Constr
      { params = [ elt, () ]
      ; module_ =
          List.map ~f:Module_name.of_string [ "Ldiffable"; "Set_diff" ]
          |> Longident_helper.of_simple_list
      ; type_name = Type_name.t
      }
  in
  let get = [%expr Ldiffable.Set_diff.get] in
  let apply_exn = [%expr Ldiffable.Set_diff.apply_exn] in
  { Core_diff.diff_type; functions = { get; apply_exn } }
;;
