@@ portable

open Base

module Stable : sig
  module V1 : sig
    module Change : sig
      type 'a t =
        | Add of 'a
        | Remove of 'a
      [@@deriving quickcheck]
    end

    type 'a t = 'a Change.t list [@@deriving bin_io, quickcheck, sexp]

    val get : from:('a, 'cmp) Set.t -> to_:('a, 'cmp) Set.t -> local_ 'a t Optional_diff.t
    val apply_exn : ('a, 'cmp) Set.t -> 'a t -> ('a, 'cmp) Set.t
    val of_list_exn : 'a t list -> local_ 'a t Optional_diff.t

    module Make (S : sig
        module Elt : sig
          type t
          type comparator_witness
        end

        type t = (Elt.t, Elt.comparator_witness) Set.t
      end) : Diff_intf.S_plain with type derived_on := S.t and type t := S.Elt.t t
  end
end

include module type of Stable.V1 with type 'a Change.t = 'a Stable.V1.Change.t
