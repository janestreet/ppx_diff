open Base
open Printf
open Ppxlib

let create ?(inlined = false) tuple ~builder ~create_core =
  let max, pos = Diffable.Tuples.max_supported in
  let open (val builder : Builder.S) in
  let n = List.length tuple in
  if n > max
  then
    raise_error
      (sprintf
         "tuples of size > %i are not supported; to increase this limit, edit %s:%i"
         max
         pos.pos_fname
         pos.pos_lnum);
  let open (val builder : Builder.S) in
  let param_diffs = List.map tuple ~f:create_core in
  let module_ =
    List.map
      ~f:Module_name.of_string
      ([ sprintf "Tuple%i" n ] @ if inlined then [ "Local" ] else [])
    @ [ Module_name.diff_module_name ~type_to_diff_name:Type_name.t ]
  in
  (* (Diff_of_type1.t, Diff_of_type2.t, 'a_diff) TupleN.Diff.t *)
  let diff_type =
    Type_kind.Constr
      { params = List.map param_diffs ~f:(fun diff -> Core_diff.diff_type diff, ())
      ; module_ = Longident_helper.of_simple_list module_
      ; type_name = Type_name.t
      }
  in
  (* [TupleN.Diff.{get,apply,of_exn}] *)
  let fn_name fn =
    List.map module_ ~f:Module_name.to_string
    |> Longident_helper.of_simple_list
    |> Longident_helper.add_suffix ~suffix:[ Function_name.to_string fn ]
    |> Longident_helper.to_expression ~builder
  in
  let fn name f =
    (* [TupleN.Diff.get get_fst_param get_snd_param ...] *)
    pexp_apply
      (fn_name name)
      (List.map param_diffs ~f:(fun diff -> Nolabel, f diff.functions))
  in
  let get = fn Function_name.get (fun f -> f.get) in
  let apply_exn = fn Function_name.apply_exn (fun f -> f.apply_exn) in
  let of_list_exn = fn Function_name.of_list_exn (fun f -> f.of_list_exn) in
  { Core_diff.diff_type; functions = { get; apply_exn; of_list_exn } }
;;
