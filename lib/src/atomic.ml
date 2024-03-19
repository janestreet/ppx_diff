open Base

module Make_base_diff (M : sig
  type t [@@deriving equal]
end) =
struct
  let[@inline] get ~from ~to_ =
    if phys_equal from to_ || M.equal from to_
    then Optional_diff.none
    else Optional_diff.return to_
  ;;

  let[@inline] apply_exn _ t = t

  let[@inline] of_list_exn = function
    | [] -> Optional_diff.none
    | _ :: _ as l -> Optional_diff.return (List.last_exn l)
  ;;
end

module Make_diff_plain (M : sig
  type t [@@deriving equal]
end) =
struct
  type derived_on = M.t
  type t = M.t [@@deriving equal]

  include Make_base_diff (M)
end

module Make_diff (M : sig
  type t [@@deriving sexp, bin_io, equal]
end) =
struct
  type derived_on = M.t
  type t = M.t [@@deriving sexp, bin_io, equal]

  include Make_base_diff (M)
end

module Make_plain (M : sig
  type t [@@deriving equal]
end) =
struct
  module Diff = Make_diff_plain (M)
end

module Make (M : sig
  type t [@@deriving equal, sexp, bin_io]
end) =
struct
  module Diff = Make_diff (M)
end
