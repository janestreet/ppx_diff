open Core
include Identifiable

val get : t
val apply_exn : t
val function_of_var : t -> Var.t -> Build_helper.t
