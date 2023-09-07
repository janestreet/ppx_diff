module type S = Comparable_intf.S

module Make (M : Core.Comparable.S_binable) = struct
  include M

  module Set = struct
    include M.Set

    module Diff = struct
      type derived_on = M.Set.t [@@deriving sexp, bin_io]
      type t = M.Set.Elt.t Set_diff.t [@@deriving sexp, bin_io]

      let get = Set_diff.get
      let apply_exn = Set_diff.apply_exn
    end
  end

  module Map = struct
    include M.Map

    module Diff = struct
      type 'v derived_on = 'v M.Map.t [@@deriving sexp, bin_io]

      type nonrec ('v, 'v_diff) t = (M.Map.Key.t, 'v, 'v_diff) Map_diff.t
      [@@deriving sexp, bin_io]

      let get = Map_diff.get
      let apply_exn = Map_diff.apply_exn
    end
  end
end
