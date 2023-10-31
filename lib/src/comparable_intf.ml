module type S = sig
  include Core.Comparable.Map_and_set_binable

  module Set : sig
    include module type of Set

    include
      Diffable_intf.S with type t := t and type Diff.t = Set.Elt.t Set_diff.Stable.V1.t
  end

  module Map : sig
    include module type of Map

    include
      Diffable_intf.S1
        with type 'a t := 'a t
         and type ('a, 'a_diff) Diff.t = (Map.Key.t, 'a, 'a_diff) Map_diff.Stable.V1.t
  end
end

module type Comparable = sig
  module Stable : sig
    module V1 : sig
      module type S = S

      module Make (M : Core.Comparable.S_binable) :
        S with type t := M.t and type comparator_witness := M.comparator_witness
    end
  end

  include module type of Stable.V1
end
