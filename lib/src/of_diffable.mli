module type Conv = sig
  type diffable
  type t

  val to_diffable : t -> diffable
  val of_diffable : diffable -> t
end

module Make (Diffable : Diffable_intf.S) (M : Conv with type diffable = Diffable.t) :
  Diffable_intf.S with type t := M.t and type Diff.t = Diffable.Diff.t

module Make_plain
    (Diffable : Diffable_intf.S_plain)
    (M : Conv with type diffable = Diffable.t) :
  Diffable_intf.S_plain with type t := M.t and type Diff.t = Diffable.Diff.t
