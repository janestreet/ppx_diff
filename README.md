ppx_diff
========

Generation of diffs and update functions for ocaml types.

`ppx_diff` is a ppx rewriter that generates the implementation of [Diffable.S].
The [Diff.t] type represents differences between two values. The [Diff.get] and
[Diff.apply_exn] functions compute and apply the differences.

Users of `ppx_diff` should add the [diffable] library to the dependencies in
[jbuild].

# Usage

## Basic usage
If you define a type as follows:

<!--BEGIN type_decl-->
```ocaml
type t [@@deriving diff]
```
<!--END-->

then code will be generated matching the following signature:

<!--BEGIN generated_sig-->
```ocaml
module Diff : sig
  type derived_on = t
  type t

  val get : from : derived_on -> to_ : derived_on -> local_ t Optional_diff.t
  val apply_exn : derived_on -> t -> derived_on

  val of_list_exn : t list -> local_ t Optional_diff.t
end
```
<!--END-->

Use of `[@@deriving diff]` in an .mli will extend the signature with the above module.
In an .ml, definitions will be generated.

## Serialization and deserialization functions

You will likely want your diff type to derive some subset of `bin_io`, `sexp`, `of_sexp`,
`sexp_of`.

By default `[@@deriving diff]` will include the same subset of the above as those
included on your original type.

So e.g. if you define a type as follows:

<!--BEGIN type_decl-->
```ocaml
type t [@@deriving sexp, bin_io, diff]
```
<!--END-->

then code will be generated matching the following signature:

<!--BEGIN generated_sig-->
```ocaml
module Diff : sig
  type derived_on = t
  type t [@@deriving sexp, bin_io]

  val get : from : derived_on -> to_ : derived_on -> local_ t Optional_diff.t
  val apply_exn : derived_on -> t -> derived_on

  val of_list_exn : t list -> local_ t Optional_diff.t
end
```

If you want to derive additional items on your diff type, you can achieve this using the
`extra_derive` argument.

For instance, you can write:

<!--BEGIN type_decl-->
```ocaml
type t [@@deriving sexp_of, diff ~extra_derive:[of_sexp ; bin_io]]
```
<!--END-->


## Signature explanation

### Optional Diff
`Optional_diff` has the following accessor functions:

<!--BEGIN generated_sig-->
```ocaml
module Optional_diff : sig
  type 'a t

  val is_none : local_ _ t -> bool
  val unsafe_value : local_ 'a t -> 'a
  val to_option : local_ 'a t -> 'a option
end
```
<!--END-->

It is designed to improve allocations. Specifically, if you have a `local_ 'a t`, that
means that `'a` was allocated, but the pointer to `'a option` was not.

### Exn functions

Why are the function called `apply_exn` and `of_list_exn` instead of  `apply` and `of_list`?

Rest assured that calling

```ocaml
Option.iter (Diff.get ~from ~to_ |> Optional_diff.to_option) ~f:(Diff.apply_exn from)
```

will never raise.


However, applying a hand-crafted diff might raise for some types (in particular for
variants when the diff is for an unexpected variant). Similarly, hand-crafing diffs using `of_list_exn` may raise if trying to combine diffs for different variants.

## Nesting
By default, diffs will be "nested". This means if a type deriving `diff` references other
types, those types must also be diffable.

For example, for the following to work

<!--BEGIN generated_sig-->
```ocaml
type t =
  { x : X.t
  ; y : Y.t
  ; z : Z.t
  } [@@deriving diff]
```
<!--END-->

`X.t`, `Y.t` and `Z.t` must also either derive `diff` or otherwise implement `Diffable.S`.

Some types are already supported, so it's ok to reference those. This includes:

- basic types: `int`, `float`, `string`, `bool`, `char`, `unit` and `option`
- `Set` and `Map` created using the various `Comparable.Make` functors

## Parametric types

`ppx_diff` works also with parametric polymorphic types. In this case the
[Diff.get] and [Diff.apply_exn] functions require diff functions for all the types in order of their
appearance in the type definition. This is similar to what you may be used to with
e.g. `t_of_sexp` functions.

The `[Diff.of_list_exn]` function requires `[of_list_exn]` and `[apply_exn]` of all the
types in order of their appearance. (The `[apply_exn]` is needed for variant types. If you
combine something like `[Set_to_variant_X of a]` with `[Diff_variant_X of a_diff]`, the
result should be `[Set_to_variant_X (apply_a_exn a a_diff)]`)

If you define a type as follows:

<!--BEGIN type_decl-->
```ocaml
type ('a, 'b) t [@@deriving diff]
```
<!--END-->

then code will be generated matching following signature:

<!--BEGIN generated_sig-->
```ocaml
module Diff : sig
  type ('a, 'b) derived_on = ('a, 'b) t

  type ('a, 'b, 'a_diff, 'b_diff) t

  val get
    :  (from:'a -> to_:'a -> local_ 'a_diff Optional_diff.t)
    -> (from:'b -> to_:'b -> local_ 'b_diff Optional_diff.t)
    -> from:('a, 'b) derived_on
    -> to_:('a, 'b) derived_on
    -> local_ ('a, 'b, 'a_diff, 'b_diff) t Optional_diff.t

  val apply_exn
    :  ('a -> 'a_diff -> 'a)
    -> ('b -> 'b_diff -> 'b)
    -> ('a, 'b) derived_on
    -> ('a, 'b, 'a_diff, 'b_diff) t
    -> ('a, 'b) derived_on

  val of_list_exn
    :  ('a_diff list -> local_ 'a_diff Optional_diff.t)
    -> ('a -> 'a_diff -> 'a)
    -> ('b_diff list -> local_ 'b_diff Optional_diff.t)
    -> ('b -> 'b_diff -> 'b)
    -> ('a, 'b, 'a_diff, 'b_diff) t list
    -> local_ ('a, 'b, 'a_diff, 'b_diff) t Optional_diff.t
end
```
<!--END-->

## Types with names other than `t`
`ppx_diff` works also with types named something else than `t`.

If you define a type as follows

<!--BEGIN type_decl-->
```ocaml
type name [@@deriving diff]
```
<!--END-->

then code will be generated matching the following signature:

<!--BEGIN generated_sig-->
```ocaml
module Diff_of_name : sig
  type derived_on = name
  type t

  val get : from : derived_on -> to_ : derived_on -> local_ t Optional_diff.t
  val apply_exn : derived_on -> t -> derived_on
  val of_list_exn : t list -> local_ t Optional_diff.t
end
```
<!--END-->

# Supported types

Below you can see some examples of what the diff types looks like for various
supported types.

## Records

The diff of a record is a list of diffs of its fields (and includes only the fields that changed)

<!--BEGIN generated_sig-->
```ocaml
type t =
  { x : X.t
  ; y : Y.t
  ; z : Z.t
  } [@@deriving diff]

module Diff : sig

  module Field_diff : sig
    type t = | X of X.Diff.t | Y of Y.Diff.t | Z of Z.Diff.t
  end

  type t = private Field_diff.t list

  val create : ?x:X.Diff.t -> ?y:Y.Diff.t -> ?z:Z.Diff.t -> unit -> t

  val create_of_variants
    :  x:local_ (X.Diff.t, Field_diff.t) Of_variant.t
    -> y:local_ (Y.Diff.t, Field_diff.t) Of_variant.t
    -> z:local_ (Z.Diff.t, Field_diff.t) Of_variant.t
    -> t

  val singleton : Field_diff.t -> t
  ...
end
```
<!--END-->

where

<!--BEGIN generated_sig-->
```ocaml
module Of_variant : sig
  type ('a, 'diff) t = ('a -> 'diff) Variantslib.Variant.t -> local_ 'a Optional_diff.t
end
```
<!--END-->

### Invariant
The `Diff.t` type is private, because the list satisfies two invariants:

(@) There is at most one item of each of `X`, `Y` and `Z` variant of `Field_diff.t` in `t`
(@) The items are in sorted order

Those two invariants allow for a more performant `apply_exn` function.

### How to create the diff
You can still create items of the `Diff.t` type by calling `Diff.singleton`, `Diff.create`,
`Diff.create_of_variants` or `Diff.of_list_exn`

Note that `Diff.of_list_exn` will correctly order the diffs and combine diffs for the same
field in order to satisfy the invariants.

### Record with one field
There is a special case for a record with just one field. We don't return a list, just the
(optional) single field diff:

<!--BEGIN generated_sig-->
```ocaml
type t = { x : X.t } [@@deriving diff]

module Diff : sig
  type t = | X of X.Diff.t
  ...
end
```
<!--END-->


## Tuples

Very similar to records

<!--BEGIN generated_sig-->
```ocaml
type t = X.t * Y.t [@@deriving diff]

module Diff : sig
  type t = (X.t, Y.t, X.Diff.t, Y.Diff.t) Diffable.Tuples.Tuple2.Diff.t
  ...
end
```
<!--END-->

where

<!--BEGIN generated_sig-->
```ocaml
module Tuple2 : sig
  type ('a1, 'a2) t = 'a1 * 'a2 [@@deriving sexp, bin_io]

  module Diff : sig
    type ('a1, 'a2) derived_on = ('a1, 'a2) t

    module Entry_diff : sig
      type ('a1, 'a2, 'a1_diff, 'a2_diff) t =
        | T1 of 'a1_diff
        | T2 of 'a2_diff
      [@@deriving variants, sexp, bin_io]
    end

    type ('a1, 'a2, 'a1_diff, 'a2_diff) t =
      private
      ('a1, 'a2, 'a1_diff, 'a2_diff) Entry_diff.t list
    [@@deriving sexp, bin_io]

    val create : ?t1:'a1_diff -> ?t2:'a2_diff -> unit -> ('a1, 'a2, 'a1_diff, 'a2_diff) t

    val create_of_variants
      :  t1:local_ ('a1_diff, ('a1, 'a2, 'a1_diff, 'a2_diff) Entry_diff.t) Of_variant.t
      -> t2:local_ ('a2_diff, ('a1, 'a2, 'a1_diff, 'a2_diff) Entry_diff.t) Of_variant.t
      -> ('a1, 'a2, 'a1_diff, 'a2_diff) t

    val singleton
      :  ('a1, 'a2, 'a1_diff, 'a2_diff) Entry_diff.t
      -> ('a1, 'a2, 'a1_diff, 'a2_diff) t

    ...
  end
end
```
<!--END-->

Again the `Entry_diff.t` entries in `Diff.t` must be sorted and unique, and again `create`,
`create_of_variants`, `singleton` functions are provided for convenience.

Tuples of size greater than `6` are not supported, but this can be trivially extended as
the `Tuple` modules are generated using cinaps.

## Constructors

<!--BEGIN generated_sig-->
```ocaml
type t = X.t [@@deriving diff]

module Diff : sig
  type t = X.Diff.t
  ...
end
```
<!--END-->

including parametrized ones

<!--BEGIN generated_sig-->
```ocaml
type t = Y.t X1.t [@@deriving diff]

module Diff : sig
  type t = (Y.t, Y.Diff.t) X.Diff.t
  ...
end
```
<!--END-->

## Vars

<!--BEGIN generated_sig-->
```ocaml
type 'a t = 'a [@@deriving diff]

module Diff : sig
  type ('a, 'a_diff) t = 'a_diff
  ...
end
```
<!--END-->

## Variants

<!--BEGIN generated_sig-->
```ocaml
type t =
  | A
  | B of B.t
[@@deriving diff]

module Diff : sig
  type t = | Set_to_a | Set_to_b of B.t | Diff_b of B.Diff.t

  ...
end
```
<!--END-->

### Inlined tuples and records

Inlined tuples and records are also supported.
Their diff types look exactly like those for regular tuples / records.

<!--BEGIN generated_sig-->
```ocaml
type t =
  | C of C1.t * C2.t
  | D of { d1 : D1.t ; d2 : D2.t}
[@@deriving diff]

module Diff : sig

  module D_record : sig
    type t =
      { global_ d1 : D1.t
      ; global_ d2 : D2.t
      }

    module Diff : sig
      type derived_on = t

      module Field_diff : sig
        type t =
          | D1 of D1.Diff.t
          | D2 of D2.Diff.t
      end

      type t = private Field_diff.t list

      val get
        :  from:local_ derived_on
        -> to_:local_ derived_on
        -> local_ t Optional_diff.t

      val apply_exn : local_ derived_on -> t -> local_ derived_on

      val of_list_exn : t list -> local_ t Optional_diff.t

      val create : ?d1:D1.Diff.t -> ?d2:D2.Diff.t -> unit -> t

      val create_of_variants
        :  d1:local_ (D1.Diff.t, Field_diff.t) Of_variant.t
        -> d2:local_ (D2.Diff.t, Field_diff.t) Of_variant.t
        -> t

      val singleton : Field_diff.t -> t
  end

  type t =
    | Set_to_c of C1.t * C2.t
    | Set_to_d of
        { d1 : D1.t
        ; d2 : D2.t
        }
    | Diff_c of Diffable.Tuples.Tuple2.For_inlined_tuple.Diff.t
    | Diff_d of D_record.Diff.t

  ...
end
```
<!--END-->
where

```ocaml
type Diffable.Tuples.Tuple2.For_inlined_tuple.Diff.t = Diffable.Tuples.Tuple2.Diff.t
```

Notice that for inlined records the ppx generates a helper module which looks the same as a regular record
module with `[@@deriving diff]`, but can be calculated from / applied to `local_` values
of `derived_on`.

Similar helper modules exist for tuples in `Diffable.Tuples.TupleN.For_inlined_tuple`


# Referencing other types

Since diffs are by default nested, any type you reference must also derive `diff` or
otherwise implement `Diffable.S`

For example, if `Some_type` doesn't implement `Diffable.S`, then the following:

```ocaml
type t = Some_type.t * Some_type.t
[@@deriving diff]
```

Will give you an error like:

```ocaml
Error: Unbound module Some_type.Diff
```

Often it's enough to just add `[@@deriving diff]` to the referenced type.

But occasionally that doesn't quite work. This section covers some special cases and how
to handle them.


## Atomic diffs

Some types, e.g. any `Identifiable`, or indeed `Some_type.t`, are super simple, and
what we really want is to have `type Diff.t = t` and only return a diff if the two values
aren't equal. We call such diffs `atomic`.

This section explains how you can mark your types as atomically diffable

### [@diff.atomic]

You can annotate any part of your type to use atomic diffs using the `[@diff.atomic]`
attribute.

For instance, the error from the example above will go away if you write

```ocaml
type t =
  (Some_type.t [@diff.atomic]) * (Some_type.t [@diff.atomic])
[@@deriving diff]
```

The annotation also works for record fields:

```ocaml
type t =
  { start : Some_type.t [@diff.atomic]
  ; stop : Some_type.t [@diff.atomic]
  }
[@@deriving diff]
```

and for variants:

```ocaml
type t =
  | Start of Some_type.t [@diff.atomic]
  | Stop of Some_type.t [@diff.atomic]
[@@deriving diff]
```

The two examples above are in fact equivalent to

```ocaml
type t =
  { start : (Some_type.t [@diff.atomic])
  ; stop : (Some_type.t [@diff.atomic])
  }
  [@@deriving diff]
```
and

```ocaml
type t =
  | Start of (Some_type.t [@diff.atomic])
  | Stop of (Some_type.t [@diff.atomic])
  [@@deriving diff]
```

but slightly nicer to write.

### ~how:"atomic"

Finally, you can mark your whole type atomic by using the `how` parameter:

```ocaml
type t = Some_type.t [@@deriving equal, diff ~how:"atomic"]
```

Any type that you mark `atomic`, must also derive / otherwise implement `equal`.


### Atomic types
To avoid having to add the annotations everywhere, you can also make the referenced type
implement `Diffable.S` in an atomic fashion.

To do this, use `Diffable.Make_atomic`, e.g.

```ocaml
module Id : sig
  type t [@@deriving sexp, bin_io]
  include Diffable.S with type t := t and type Diff.t = t
end = struct
  type t = string [@@deriving sexp, bin_io, equal]
  include functor Diffable.Atomic.Make
end
```

Note that `Diffable.Atomic.Make` requires that your type derives `equal`, `sexp` and `bin_io`.

If you don't derive all of those (but do derive, say, `sexp_of`), `Diffable.Atomic` will
still work but you will need to be more verbose, e.g.

```ocaml
module Id : sig
  type t [@@deriving sexp_of]

  module Diff : sig
    type derived_on = t [@@deriving sexp_of]
    type t = derived_on [@@deriving sexp_of]

    include Diffable.Diff.S_plain with type t := t and type derived_on := derived_on
  end
end = struct
  type t = some_type [@@deriving equal, sexp_of]

  module Diff = struct
    type derived_on = t [@@deriving equal, sexp_of]
    type t = derived_on [@@deriving equal, sexp_of]

    include functor Diffable.Atomic.Make_diff_plain
  end
end
```

### Atomic using compare

If a type doesn't implement `equal`, but does implement `compare`, you can also use the
`atomic_using_compare` attribute, which works in exactly the same way as `atomic`.

E.g. the following will work:

```ocaml
type t = Some_type.t [@@deriving compare, diff ~how:"atomic_using_compare"]
```

```ocaml
type t =
  { start : Some_type.t [@diff.atomic_using_compare]
  ; stop : Some_type.t [@diff.atomic_using_compare]
  }
[@@deriving diff]
```

`atomic_using_compare` does not work in mlis/signatures, just continue using `atomic`
there - the generated signature is exactly the same


### A word of warning about NANs

It may come as a surprise, but in Core the following holds

```ocaml
Float.equal Float.nan Float.nan = false
Float.compare Float.nan Float.nan = 0
```

If you want to avoid spurious NANs in your diffs, e.g. because you want to manually
inspect your diffs and don't want them too be noisy, or because you expect there to be a
lot of NANs in your values, you need to make sure you use `compare` and not `equal`

In practice this means you'll want to:

1. Use `atomic_using_compare` and not `atomic`
2. If using `Diffable.Atomic.Make` add an override `let equal = [%compare.equal]`

The primitive `float` type is already handled using `compare`


## Sets and maps

Sets and maps implemented using the various `Make` modules in `Comparable` and
`Comparable.Stable` already implement `Diffable.S` and `Diffable.S1` respectively, so you
can use them out of the box.

All of the following will work:

```
type t = My_id.Set.t [@@deriving diff]
type t = My_id.Stable.V1.Set.t [@@deriving diff]
type t = { value : float My_id.Map.t } [@@deriving diff]
type t = { value : float My_id.Stable.V1.Map.t } [@@deriving diff]
type 'a t = | Value of 'a My_id.Map.t [@@deriving diff]
type 'a t = | Value of 'a My_id.Stable.V1.Map.t [@@deriving diff]
```

The diff for sets looks like:

```ocaml
module Diff : sig
  type derived_on = t
  type t = My_id.t Diffable.Set_diff.t
  ...
end
```

where

```ocaml
module Diffable.Set_diff : sig
  module Change : sig
    type 'a t = | Add of 'a | Remove of 'a
  end

  type 'a t = 'a Change.t list
end
```

and the diff for maps looks like:

```ocaml
module Diff : sig
  type 'v derived_on = 'v t
  type ('v, 'v_diff) t = (My_id.t, 'v, 'v_diff) Diffable.Map_diff.t
  ...
end
```

where

```ocaml
module Diffable.Map_diff : sig
  module Change : sig
    type ('key, 'v, 'v_diff) t =
      | Remove of 'key
      | Add of 'key * 'v
      | Diff of 'key * 'v_diff
    [@@deriving sexp, bin_io]
  end

  type ('key, 'v, 'v_diff) t = ('key, 'v, 'v_diff) Change.t list [@@deriving sexp, bin_io]
end
```

### Map value diffs

By default the values in a map are also diffed.

So if the following:

```ocaml
type t = Some_type.t My_id.Map.t
```

gives you an error

```ocaml
Error: Unbound module Some_type.Diff
```

you can fix it by writing


```ocaml
type t = (Some_type.t [@diff.atomic]) My_id.Map.t
```

### Sets and maps with applicative functors

Applicative functors are not supported by diff in general, so none of the following will work:

```ocaml
type t = Set.M(Int).t [@@deriving diff]
type t = float Map.M(Int).t [@@deriving diff]
type t = Set.Stable.V1.M(Int).t [@@deriving diff]
type t = float Map.Stable.V1.M(Int).t [@@deriving diff]
```

However, there is specific support for applicative functors for sets and maps: you can use
`set` and `map` annotations similar to the `atomic` annotation.

Here are some examples of what will work:

```ocaml
type t = Set.M(Int).t [@@deriving diff ~how:"set"]
type t = (float Map.M(Int).t [@diff.map]) [@@deriving diff]
type t = { value : Set.Stable.V1.M(Int).t [@diff.set] } [@@deriving diff]
type t = | Value of float Map.Stable.V1.M(Int).t [@diff.map] [@@deriving diff]
```

### Comparator witness

The following are not supported by default:

```ocaml
type t = (int, Int.comparator_witness) Set.t [@@deriving diff]
type t = (int, float, Int.comparator_witness) Map.t [@@deriving diff]
```

But they do work with the `set` and `map` annotations.

Here are some examples of what will work:

```ocaml
type t = (int, Int.comparator_witness) Set.t [@@deriving diff ~how:"set"]
type t = (int, float, Int.comparator_witness) Map.t [@@deriving diff ~how:"map"]
type t = { value : (int, Int.comparator_witness) Set.t [@diff.set] } [@@deriving diff]
type t = 
  | Value of (int, float, Int.comparator_witness) Map.t [@diff.map] 
[@@deriving diff]
```

### General set/map diff

For the set/map annotations to work in general, they need to be able to determine the
element type of the set / the key type of the map. 

#### Default

All of the examples so far use a heuristic to determine the key/elt. 
By default we assume that the key/elt is `X.t` for:

- any applicative functor with `X` as an argument (e.g. `Set.M(X).t`, `Map.Stable.V1.M(X).t`)
- any type called `X.Set.t` or `X.Map.t`
- any type where `X.t` is the first of two parameters for set or first of three for map (so
  that it works for `(X.t, X.comparator_witness) Set.t` or `(X.t, Y.t,
  X.comparator_witness) Map.t`))

#### Overrides

If none of the above heuristics apply (or if you want to override the default for whatever
reason), you will need to provide the key/elt explicitly.

Any of the following would work:

```ocaml
type t = My_int_set.t [@@deriving diff ~how:"set" ~elt:int]
type t = float My_int_map.t [@@deriving diff ~how:"map" ~key:int]
type t = { value : My_int_set.t [@diff.set (elt : int)] } [@@deriving diff]
type t = | Value of float My_int_map.t [@diff.map (key : int)] [@@deriving diff]
```

# Abstract diffs

One problem that arises from using `atomic`, `atomic_using_compare`, `set` and `map` attributes is that
your type definition may look a bit polluted, since you need to use the attributes in both
the mli and the ml

E.g. the following is rather verbose

```ocaml
module Range : sig
  type t =
    { start : Some_type.t [@diff.atomic]
    ; stop : Some_type.t [@diff.atomic]
    }
  [@@deriving diff]
end = struct
  type t =
    { start : Some_type.t [@diff.atomic]
    ; stop : Some_type.t [@diff.atomic]
    }
  [@@deriving diff]
end
```

If you don't care about exposing what the diff type actually is, you can make the diff
abstract.

You can do this either directly by writing:

```ocaml
module Range : sig
  type t =
    { start : Some_type.t
    ; stop : Some_type.t
    }

  include Diffable.S with type t := t
end
```

or, equivalently, by using `~how:"abstract"`:

```ocaml
module Range : sig
  type t =
    { start : Some_type.t
    ; stop : Some_type.t
    }
  [@@deriving diff ~how:"abstract"]
end
```

# Stability

Want to use `[@@deriving diff]` with stable types and guarantee that `[Diff.t]` is also stable?
You can use the `[stable_version]` annotation, e.g.

```ocaml
type t = ... [@@deriving diff ~stable_version:1]
```

The resulting diff type won't change as long as it only references other types that are
both stable themselves and use the diff `[stable_version]` annotation (or atomic diffs).
Note that the compiler does NOT check that you got this right, so we suggest adding a bin
digest test for peace of mind.

Only version `1` exists at the moment. There are also currently no plans to change any the
types generated by diff, but the `stable_version` annotation will protect you in case we
ever do choose to make changes.


# Polling state RPC

`ppx_diff` can be used with `Polling_state_rpc` by calling `Diffable_polling_state_rpc_response.Polling_state_rpc_response.Make`

For example, the following will work:

```ocaml
module Query = Unit

module Response = struct
  type t =
    { foo : int
    ; bar : float
    ; baz : char
    } [@@deriving sexp, bin_io, diff]
end

let polling_state_rpc =
    Polling_state_rpc.create
      ~name:"get-response"
      ~version:0
      ~query_equal:Query.equal
      ~bin_query:Query.bin_t
      (module Diffable_polling_state_rpc_response.Polling_state_rpc_response.Make
           (Response))
```
