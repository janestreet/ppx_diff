(* Represents a type declaration, e.g.

   in [type ('a, 'b) t = 'a option]:

   [params = ['a ; 'b]]
   [name = t]
   [kind = 'a option]
*)
open! Base
open Ppxlib

module Flags : sig
  type t =
    { private_ : bool
    ; nonrec_ : bool
    }
end

type 'extra t =
  { params : Type_param.t list
  ; name : Type_name.t
  ; kind : 'extra Type_kind.t
  ; unboxed : bool
  }

val create
  :  type_declaration
  -> builder:Builder.t
  -> how_to_diff:How_to_diff.Maybe_abstract.t
  -> How_to_diff.t t * How_to_diff.Atomic.t option

val to_items : ?flags:Flags.t -> unit t -> context:Context.t -> Items.t
val pointer : ?module_:Module_name.t list -> _ t -> unit Type_kind.core
val map : 'a t -> f:('a -> 'b) -> 'b t
