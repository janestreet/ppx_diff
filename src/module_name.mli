open Base
include Identifiable.S

val diff_module_name : type_to_diff_name:Type_name.t -> t

(* [prefix] if [type_name = t], otherwise [prefix]_of_[type_name] *)
val generate : prefix:string -> type_name:Type_name.t -> t
