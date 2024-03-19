open Base
open Base_quickcheck.Export
open Bin_prot.Std

module type S_with_quickcheck = sig
  type t [@@deriving quickcheck]

  include Diff_intf.S with type t := t
end

module Make_atomic_with_quickcheck (M : sig
  type t [@@deriving sexp, bin_io, equal, quickcheck]
end) =
struct
  include Atomic.Make_diff (M)

  type t = M.t [@@deriving quickcheck]
end

module Diff_of_bool = Make_atomic_with_quickcheck (struct
  type t = bool [@@deriving sexp, bin_io, equal, quickcheck]
end)

module Diff_of_char = Make_atomic_with_quickcheck (struct
  type t = char [@@deriving sexp, bin_io, equal, quickcheck]
end)

module Diff_of_float = Make_atomic_with_quickcheck (struct
  type t = float [@@deriving sexp, bin_io, compare, quickcheck]

  (* Overriding [equal], because
       - [Float.equal Float.nan Float.nan = false]
       - [Float.compare Float.nan Float.nan = 0]
         The latter makes more sense for diffs
    *)
  let equal = [%compare.equal: t]
end)

module Diff_of_int = Make_atomic_with_quickcheck (struct
  type t = int [@@deriving sexp, bin_io, equal, quickcheck]
end)

module Diff_of_string = Make_atomic_with_quickcheck (struct
  type t = string [@@deriving sexp, bin_io, equal, quickcheck]
end)

module Diff_of_unit = Make_atomic_with_quickcheck (struct
  type t = unit [@@deriving sexp, bin_io, equal, quickcheck]
end)

module Diff_of_option = struct
  type 'a derived_on = 'a option [@@deriving sexp, bin_io]

  type ('a, 'a_diff) t =
    | Set_to_none
    | Set_to_some of 'a
    | Diff_some of 'a_diff
  [@@deriving sexp, bin_io, quickcheck]

  let get get_a ~from ~to_ =
    if phys_equal from to_
    then Optional_diff.none
    else (
      match from, to_ with
      | None, None -> Optional_diff.none
      | Some from, Some to_ ->
        Optional_diff.map (get_a ~from ~to_) ~f:(fun d -> Diff_some d)
      | None, Some x -> Optional_diff.return (Set_to_some x)
      | Some _, None -> Optional_diff.return Set_to_none)
  ;;

  let apply_exn apply_a_exn derived_on diff =
    match derived_on, diff with
    | _, Set_to_some x -> Some x
    | _, Set_to_none -> None
    | Some derived_on, Diff_some diff -> Some (apply_a_exn derived_on diff)
    | None, Diff_some _ ->
      raise_s
        [%message
          "Could not apply diff. Variant mismatch." ~derived_on:"None" ~diff:"Diff_some"]
  ;;

  let of_list_exn of_list_exn_a apply_a_exn diffs =
    match diffs with
    | [] -> Optional_diff.none
    | [ hd ] -> Optional_diff.return hd
    | l ->
      let trailing_diffs_rev, rest_rev =
        List.rev l
        |> List.split_while ~f:(function
             | Diff_some _ -> true
             | Set_to_some _ | Set_to_none -> false)
      in
      let a_diffs =
        List.rev_map trailing_diffs_rev ~f:(function
          | Diff_some a_diff -> a_diff
          | Set_to_none | Set_to_some _ -> assert false)
      in
      (match rest_rev, a_diffs with
       | [], [] | Diff_some _ :: _, _ -> assert false
       | ((Set_to_none | Set_to_some _) as t) :: _, [] -> Optional_diff.return t
       | [], a_diffs ->
         let%map.Optional_diff a_diff = of_list_exn_a a_diffs in
         Diff_some a_diff
       | Set_to_some a :: _, a_diffs ->
         Optional_diff.return (Set_to_some (List.fold a_diffs ~init:a ~f:apply_a_exn))
       | Set_to_none :: _, _ :: _ ->
         raise_s
           [%message
             "Could not combine diffs. Variant mismatch."
               ~first_diff:"Set_to_none"
               ~second_diff:"Diff_some"])
  ;;
end
