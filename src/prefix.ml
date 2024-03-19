open Base

type t = string

let derived_on = "derived_on"
let diff = "diff"
let from = "from"
let to_ = "to_"
let get = Function_name.(to_string get)
let apply_exn = Function_name.(to_string apply_exn)
let of_list_exn = Function_name.(to_string of_list_exn)
let to_string = Fn.id
let of_string = Fn.id
let any = "_"

let to_prefix = function
  | None -> ""
  | Some s -> if String.is_suffix s ~suffix:"_" then s else s ^ "_"
;;
