open Base

(* Makes an educated guess at the key of a map / elt of a set module.

   Will return X.t for

   a) X.Map.t (for `Map) or X.Set.t (for `Set)
   b) any applicative functor with [X] as argument (e.g. Map.M(X))
*)
val key_or_elt_heuristic
  :  module_:Module_name.t Longident_helper.t option
  -> type_name:Type_name.t
  -> [ `Map | `Set ]
  -> unit Type_kind.core_kind option
