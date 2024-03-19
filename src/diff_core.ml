open! Base

let rec create core ~context =
  let { Context.builder; sig_or_struct; _ } = context in
  let create_core = create ~context in
  let kind, how_to_diff = core in
  match (how_to_diff : How_to_diff.t) with
  | None ->
    (match (kind : _ Type_kind.core_kind) with
     | Var var -> Diff_var.create var ~builder
     | Tuple tuple -> Diff_tuple.create tuple ~builder ~create_core
     | Constr constr -> Diff_constr.create constr ~create_core ~builder
     | Polymorphic_variant variant ->
       Diff_variant.create_polymorphic variant ~create_core ~context)
  | Some (Atomic atomic) -> Diff_atomic.create_core kind ~atomic ~sig_or_struct ~builder
  | Some (As_set { elt }) -> Diff_as_set.create kind ~context ~elt
  | Some (As_map { key }) -> Diff_as_map.create kind ~context ~create_core ~key
;;
