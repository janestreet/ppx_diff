open! Base

type 'a t = 'a * 'a list

val map : 'a t -> f:('a -> 'b) -> 'b t
val to_list : 'a t -> 'a list
val append : 'a t -> 'a list -> 'a t
val drop_last : 'a t -> 'a list
val last : 'a t -> 'a
