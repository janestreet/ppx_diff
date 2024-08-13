module type Conv = sig
  type diffable
  type t

  val to_diffable : t -> diffable
  val of_diffable : diffable -> t
end

module Make (Diffable : Diffable_intf.S) (M : Conv with type diffable = Diffable.t) :
  Diffable_intf.S with type t := M.t and type Diff.t = Diffable.Diff.t = struct
  module Diff = struct
    include Diffable.Diff

    type derived_on = M.t

    let get ~from ~to_ =
      (Diffable.Diff.get [@inlined hint])
        ~from:(M.to_diffable from)
        ~to_:(M.to_diffable to_)
    ;;

    let apply_exn t diff =
      (Diffable.Diff.apply_exn [@inlined hint]) (M.to_diffable t) diff |> M.of_diffable
    ;;

    let of_list_exn diffs = Diffable.Diff.of_list_exn diffs
  end
end
