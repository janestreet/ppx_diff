open! Base

let create (var : Var.t) ~builder : Core_diff.t =
  let open (val builder : Builder.S) in
  let var_fn name = e (Function_name.function_of_var name var) in
  (* type ('a, 'a_diff) t = 'a_diff *)
  let diff_type = Var.diff_var var in
  (* get ~from ~to = _get_a ~from ~to *)
  let get = var_fn Function_name.get in
  (* apply_exn derived_on diff = _apply_a_exn derived_on diff *)
  let apply_exn = var_fn Function_name.apply_exn in
  { diff_type = Var diff_type
  ; functions = { get; apply_exn; of_list_exn = var_fn Function_name.of_list_exn }
  }
;;
