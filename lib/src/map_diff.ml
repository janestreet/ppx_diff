open Core

module Stable = struct
  module V1 = struct
    module Change = struct
      type ('k, 'v, 'v_diff) t =
        | Remove of 'k
        | Add of 'k * 'v
        | Diff of 'k * 'v_diff
      [@@deriving sexp, bin_io]
    end

    type ('k, 'v, 'v_diff) t = ('k, 'v, 'v_diff) Change.t list [@@deriving sexp, bin_io]

    let get (type a a_diff) (get_a : from:a -> to_:a -> a_diff Optional_diff.t) ~from ~to_
      =
      if phys_equal from to_
      then Optional_diff.none
      else (
        let diff =
          Map.fold_symmetric_diff
            from
            to_
            ~data_equal:phys_equal
            ~init:[]
            ~f:(fun acc (key, diff) ->
            match diff with
            | `Left _ -> Change.Remove key :: acc
            | `Right value -> Change.Add (key, value) :: acc
            | `Unequal (from, to_) ->
              let diff = get_a ~from ~to_ in
              if Optional_diff.is_none diff
              then acc
              else Change.Diff (key, Optional_diff.unsafe_value diff) :: acc)
        in
        if List.is_empty diff then Optional_diff.none else Optional_diff.return diff)
    ;;

    let apply_exn apply_a_exn derived_on diff =
      List.fold ~init:derived_on diff ~f:(fun acc -> function
        | Change.Remove key -> Map.remove acc key
        | Change.Add (key, data) -> Map.set acc ~key ~data
        | Change.Diff (key, diff) ->
          Map.set acc ~key ~data:(apply_a_exn (Map.find_exn acc key) diff))
    ;;

    module Make (M : sig
      module Key : sig
        type t
        type comparator_witness
      end

      type 'v t = (Key.t, 'v, Key.comparator_witness) Map.t
    end) :
      Diff_intf.S1_plain
        with type 'v derived_on := 'v M.t
         and type ('v, 'v_diff) t := (M.Key.t, 'v, 'v_diff) t = struct
      let get = get
      let apply_exn = apply_exn
    end
  end
end

include Stable.V1
