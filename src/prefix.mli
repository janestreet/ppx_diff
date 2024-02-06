(* Various text helpers to avoid typos *)
type t

val derived_on : t
val diff : t
val from : t
val to_ : t
val get : t
val apply_exn : t
val of_list_exn : t
val to_string : t -> string
val of_string : string -> t
val to_prefix : t option -> string

(* any = "_" *)
val any : t
