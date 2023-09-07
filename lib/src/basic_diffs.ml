open Core
module Diff_of_bool = Atomic.Make_diff (Bool)
module Diff_of_char = Atomic.Make_diff (Char)

module Diff_of_float = Atomic.Make_diff (struct
  include Float

  (* Overriding [equal], because
       - [Float.equal Float.nan Float.nan = false]
       - [Float.compare Float.nan Float.nan = 0]
         The latter makes more sense for diffs
    *)
  let equal = [%compare.equal: t]
end)

module Diff_of_int = Atomic.Make_diff (Int)
module Diff_of_string = Atomic.Make_diff (String)
module Diff_of_unit = Atomic.Make_diff (Unit)

module Diff_of_option = struct
  type 'a derived_on = 'a option [@@deriving sexp, bin_io]

  type ('a, 'a_diff) t =
    | Set_to_none
    | Set_to_some of 'a
    | Diff_some of 'a_diff
  [@@deriving sexp, bin_io]

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
end
