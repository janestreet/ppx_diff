open Base
include String

let get = "get"
let apply_exn = "apply_exn"
let of_list_exn = "of_list_exn"

let function_of_var t var =
  (* Starts with a "_", so that some functions can be unused without the compiler
     complaining.  E.g. if [type ('a, 'b) t = 'a option], then the _get_b and _apply_b
     functions will be ignored *)
  let text =
    match String.chop_suffix t ~suffix:"_exn" with
    | None -> t ^ "_" ^ Var.to_string var
    | Some prefix -> prefix ^ "_" ^ Var.to_string var ^ "_exn"
  in
  Build_helper.Text ("_" ^ text)
;;
