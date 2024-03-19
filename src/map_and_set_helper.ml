open Base

let key_or_elt_heuristic ~module_ ~type_name map_or_set =
  let return module_ =
    Type_kind.Constr { params = []; module_ = Some module_; type_name = Type_name.t }
    |> Option.return
  in
  match (module_ : Module_name.t Longident_helper.t option) with
  | None -> None
  | Some (Functor_application (_functor, arg, _)) -> return arg
  | Some (Simple l) ->
    let expected_module_name =
      match map_or_set with
      | `Map -> "Map"
      | `Set -> "Set"
    in
    if String.( <> ) (Module_name.to_string (Nonempty_list.last l)) expected_module_name
       || Type_name.(type_name <> t)
    then None
    else (
      match Nonempty_list.drop_last l with
      | [] -> None
      | hd :: tl -> return (Simple (hd, tl)))
;;
