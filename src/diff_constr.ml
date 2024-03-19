open Base
open Ppxlib

(* For a constr, e.g.

   type 'a t = (type1, type2, 'a) X.t

   the generated code looks like:

   {[
     module Diff : sig
       type 'a derived_on = 'a t
       type ('a, 'a_diff) = (type1, type2, 'a, Diff_of_type1.t, Diff_of_type2.t, 'a_diff) X.Diff.t

       let get _get_a = X.Diff.t Diff_of_type1.get Diff_of_type2.get _get_a
       let apply _apply_a = X.Diff.t Diff_of_type1.apply Diff_of_type2.apply _apply_a
     end
   ]}
*)

let create
  (constr : How_to_diff.t Type_kind.constr)
  ~(create_core : Core_diff.Create.t)
  ~builder
  : Core_diff.t
  =
  let open (val builder : Builder.S) in
  let { Type_kind.module_; type_name; params } = constr in
  let param_diffs = List.map params ~f:create_core in
  (* [X.Diff] *)
  let module_ =
    Longident_helper.to_simple_list module_ ~builder ~on_functor_application:(fun _ ->
      Error.createf
        "Functor applications are not supported (with the exception of \"set\" for \
         elements and \"map\" for keys)")
    @ [ Module_name.diff_module_name ~type_to_diff_name:type_name ]
  in
  (* (type1, type2, 'a, Diff_of_type1.t, Diff_of_type2.t, 'a_diff) X.Diff.t *)
  let diff_type =
    Type_kind.Constr
      { params =
          List.map params ~f:(Type_kind.map_core ~f:(fun _ -> ()))
          @ List.map param_diffs ~f:(fun diff -> Core_diff.diff_type diff, ())
      ; module_ = Longident_helper.of_simple_list module_
      ; type_name = Type_name.t
      }
  in
  (* [X.Diff.get] / [X.Diff.apply] *)
  let fn_name fn =
    List.map module_ ~f:Module_name.to_string
    |> Longident_helper.of_simple_list
    |> Longident_helper.add_suffix ~suffix:(Function_name.to_string fn, [])
    |> Longident_helper.to_expression ~builder
  in
  let get =
    pexp_apply
      (fn_name Function_name.get)
      (List.map param_diffs ~f:(fun diff -> Nolabel, diff.functions.get))
  in
  let apply_exn =
    pexp_apply
      (fn_name Function_name.apply_exn)
      (List.map param_diffs ~f:(fun diff -> Nolabel, diff.functions.apply_exn))
  in
  let of_list_exn =
    pexp_apply
      (fn_name Function_name.of_list_exn)
      (List.concat_map param_diffs ~f:(fun diff ->
         [ Nolabel, diff.functions.of_list_exn; Nolabel, diff.functions.apply_exn ]))
  in
  { Core_diff.diff_type; functions = { get; apply_exn; of_list_exn } }
;;
