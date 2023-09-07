open Core

module Change : sig
  type ('k, 'v, 'v_diff) t =
    | Remove of 'k
    | Add of 'k * 'v
    | Diff of 'k * 'v_diff
  [@@deriving sexp, bin_io]
end

type ('k, 'v, 'v_diff) t = ('k, 'v, 'v_diff) Change.t list [@@deriving sexp, bin_io]

val get
  :  (from:'v -> to_:'v -> 'v_diff Optional_diff.t)
  -> from:('k, 'v, 'cmp) Map.t
  -> to_:('k, 'v, 'cmp) Map.t
  -> ('k, 'v, 'v_diff) t Optional_diff.t

val apply_exn
  :  ('v -> 'v_diff -> 'v)
  -> ('k, 'v, 'cmp) Map.t
  -> ('k, 'v, 'v_diff) t
  -> ('k, 'v, 'cmp) Map.t

module Make (M : sig
  module Key : sig
    type t
    type comparator_witness
  end

  type 'v t = (Key.t, 'v, Key.comparator_witness) Map.t
end) :
  Diff_intf.S1_plain
    with type 'v derived_on := 'v M.t
     and type ('v, 'v_diff) t := (M.Key.t, 'v, 'v_diff) t
