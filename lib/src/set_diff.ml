open Base
open Bin_prot.Std

module Stable = struct
  module V1 = struct
    module Change = struct
      type 'a t =
        | Add of 'a
        | Remove of 'a
      [@@deriving sexp, bin_io]
    end

    type 'a t = 'a Change.t list [@@deriving sexp, bin_io]

    let get ~from ~to_ =
      if phys_equal from to_
      then Optional_diff.none
      else (
        let diff =
          Set.symmetric_diff from to_
          |> Sequence.to_list
          |> List.map ~f:(function
               | First a -> Change.Remove a
               | Second a -> Change.Add a)
        in
        if List.is_empty diff then Optional_diff.none else Optional_diff.return diff)
    ;;

    let apply_exn set diff =
      List.fold diff ~init:set ~f:(fun acc diff ->
        match diff with
        | Change.Remove set -> Set.remove acc set
        | Change.Add set -> Set.add acc set)
    ;;

    let of_list_exn = function
      | [] -> Optional_diff.none
      | _ :: _ as l -> Optional_diff.return (List.concat l)
    ;;

    module Make (S : sig
      module Elt : sig
        type t
        type comparator_witness
      end

      type t = (Elt.t, Elt.comparator_witness) Set.t
    end) : Diff_intf.S_plain with type derived_on := S.t and type t := S.Elt.t t = struct
      let get = get
      let apply_exn = apply_exn
      let of_list_exn = of_list_exn
    end
  end
end

include Stable.V1
