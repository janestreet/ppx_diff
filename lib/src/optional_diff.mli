@@ portable

(* Basically a ['a option], but designed to reduce allocations.

   If we have a [local_ 'a t] that means that ['a] was allocated, but ['a option] was not
*)
open! Base

type 'a t

val none : _ t
val get_none : unit -> _ t
val return : 'a -> local_ 'a t
val map : local_ 'a t -> f:local_ ('a -> 'b) -> local_ 'b t
val bind : local_ 'a t -> f:local_ ('a -> local_ 'b t) -> local_ 'b t
val both : [ `both_would_allocate__use_bind_instead ]
val ( >>| ) : local_ 'a t -> local_ ('a -> 'b) -> local_ 'b t
val ( >>= ) : local_ 'a t -> local_ ('a -> local_ 'b t) -> local_ 'b t
val is_none : local_ 'a t -> bool
val unsafe_value : local_ 'a t -> 'a
val to_option : local_ 'a t -> 'a option

module Optional_syntax : sig
  module Optional_syntax : sig
    val is_none : local_ 'a t -> bool
    val unsafe_value : local_ 'a t -> 'a
  end
end

module Let_syntax : sig
  val return : 'a -> local_ 'a t

  module Let_syntax : sig
    val return : 'a -> local_ 'a t
    val map : local_ 'a t -> f:local_ ('a -> 'b) -> local_ 'b t
    val bind : local_ 'a t -> f:local_ ('a -> local_ 'b t) -> local_ 'b t
    val both : [ `both_would_allocate__use_bind_instead ]

    module Open_on_rhs : sig end
  end
end
