open! Base

[%%template:
[@@@mode.default m = (local, global)]

module%template.portable Make_diff (M : sig
    type t [@@deriving sexp, bin_io, (equal [@mode m])]
  end) : Diff_intf.S with type derived_on = M.t and type t = M.t

module%template.portable Make_diff_plain (M : sig
    type t [@@deriving equal [@mode m]]
  end) : Diff_intf.S_plain with type derived_on := M.t and type t := M.t

module%template.portable Make (M : sig
    type t [@@deriving (equal [@mode m]), sexp, bin_io]
  end) : Diffable_intf.S with type t := M.t and type Diff.t = M.t

module%template.portable Make_plain (M : sig
    type t [@@deriving equal [@mode m]]
  end) : Diffable_intf.S_plain with type t := M.t and type Diff.t = M.t]
