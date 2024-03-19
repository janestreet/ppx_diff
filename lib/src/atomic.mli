open! Base

module Make_diff (M : sig
  type t [@@deriving sexp, bin_io, equal]
end) : Diff_intf.S with type derived_on = M.t and type t = M.t

module Make_diff_plain (M : sig
  type t [@@deriving equal]
end) : Diff_intf.S_plain with type derived_on := M.t and type t := M.t

module Make (M : sig
  type t [@@deriving equal, sexp, bin_io]
end) : Diffable_intf.S with type t := M.t and type Diff.t = M.t

module Make_plain (M : sig
  type t [@@deriving equal]
end) : Diffable_intf.S_plain with type t := M.t and type Diff.t = M.t
