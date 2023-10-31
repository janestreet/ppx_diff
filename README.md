ppx_ldiff
=========

Generation of diffs and update functions for ocaml types.

`ppx_ldiff` is a ppx rewriter that generates the implementation of [Ldiffable.S].
The [Diff.t] type represents differences between two values. The [Diff.get] and
[Diff.apply_exn] functions compute and apply the differences.

Users of `ppx_ldiff` should add the [ldiffable] library to the dependencies in
[jbuild].

# Usage

## Basic usage
If you define a type as follows:

<!--BEGIN type_decl-->
```ocaml
type t [@@deriving ldiff]
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
end
```
<!--END-->

Use of `[@@deriving ldiff]` in an .mli will extend the signature with the above module.
In an .ml, definitions will be generated.

## Serialization and deserialization functions

You will likely want your diff type to derive some subset of `bin_io`, `sexp`, `of_sexp`,
`sexp_of`.

By default `[@@deriving ldiff]` will include the same subset of the above as those
included on your original type.

So e.g. if you define a type as follows:

<!--BEGIN type_decl-->
```ocaml
type t [@@deriving sexp, bin_io, ldiff]
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
end
```

If you want to derive additional items on your diff type, you can achieve this using the
`extra_derive` argument.

For instance, you can write:

<!--BEGIN type_decl-->
```ocaml
type t [@@deriving sexp_of, ldiff ~extra_derive:[of_sexp ; bin_io]]
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
  val to_option : local_ 'a t -> t option
end
```
<!--END-->

It is designed to improve allocations. Specifically, if you have a `local_ 'a t`, that
means that `'a` was allocated, but the pointer to `'a option` was not.

### Apply_exn

Why is the function called `apply_exn` and not `apply`?

Rest assured that calling

```ocaml
Option.iter (Diff.get ~from ~to_ |> Optional_diff.to_option) ~f:(Diff.apply_exn from)
```

will never raise.


However, applying a hand-crafted diff might raise for some types (in particular for
variants when the diff is for an unexpected variant, and for maps if applying diff to an
non-existing element)

## Nesting
By default, diffs will be "nested". This means if a type deriving `ldiff` references other
types, those types must also be diffable.

For example, for the following to work

<!--BEGIN generated_sig-->
```ocaml
type t =
  { x : X.t
  ; y : Y.t
  ; z : Z.t
  } [@@deriving ldiff]
```
<!--END-->

`X.t`, `Y.t` and `Z.t` must also either derive `ldiff` or otherwise implement `Ldiffable.S`.

Some basic types are already supported, in particular `int`, `float`, `string`, `bool`,
`char`, `unit` and `option`, so it is ok to reference those.

## Parametric types

`ppx_ldiff` works also with parametric polymorphic types. In this case the
[Diff.get] and [Diff.apply_exn] functions require diff functions for all the types in order of their
appearance in the type definition. This is similar to what you may be used to with
e.g. `t_of_sexp` functions.

If you define a type as follows:

<!--BEGIN type_decl-->
```ocaml
type ('a, 'b) t [@@deriving ldiff]
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
end
```
<!--END-->

## Types with names other than `t`
`ppx_ldiff` works also with types named something else than `t`.

If you define a type as follows

<!--BEGIN type_decl-->
```ocaml
type name [@@deriving ldiff]
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
  } [@@deriving ldiff]

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
  val of_field_diffs_exn : Field_diff.t list -> t
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
`Diff.create_of_variants` or `Diff.of_field_diffs_exn`

Note that `Diff.of_field_diffs_exn` will fail if there are two entries for the same `Field_diff.t` variant, but it
will be happy to reorder the entries for you to satisfy invariant `2`

### Record with one field
There is a special case for a record with just one field. We don't return a list, just the
(optional) single field diff:

<!--BEGIN generated_sig-->
```ocaml
type t = { x : X.t } [@@deriving ldiff]

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
type t = X.t * Y.t [@@deriving ldiff]

module Diff : sig
  type t = (X.t, Y.t, X.Diff.t, Y.Diff.t) Ldiffable.Tuples.Tuple2.Diff.t
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

    val of_entry_diffs_exn
      :  ('a1, 'a2, 'a1_diff, 'a2_diff) Entry_diff.t list
      -> ('a1, 'a2, 'a1_diff, 'a2_diff) t

    ...
  end
end
```
<!--END-->

Again the `Entry_diff.t` entries in `Diff.t` must be sorted and unique, and again `create`,
`create_of_variants`, `singleton` and `of_entry_diffs_exn` functions are provided for convenience.

Tuples of size greater than `6` are not supported, but this can be trivially extended as
the `Tuple` modules are generated using cinaps.

## Constructors

<!--BEGIN generated_sig-->
```ocaml
type t = X.t [@@deriving ldiff]

module Diff : sig
  type t = X.Diff.t
  ...
end
```
<!--END-->

including parametrized ones

<!--BEGIN generated_sig-->
```ocaml
type t = Y.t X1.t [@@deriving ldiff]

module Diff : sig
  type t = (Y.t, Y.Diff.t) X.Diff.t
  ...
end
```
<!--END-->

## Vars

<!--BEGIN generated_sig-->
```ocaml
type 'a t = 'a [@@deriving ldiff]

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
[@@deriving ldiff]

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
[@@deriving ldiff]

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

      val create : ?d1:D1.Diff.t -> ?d2:D2.Diff.t -> unit -> t
    
      val create_of_variants 
        :  d1:local_ (D1.Diff.t, Field_diff.t) Of_variant.t
        -> d2:local_ (D2.Diff.t, Field_diff.t) Of_variant.t 
        -> t
      
      val of_single : Field_diff.t -> t
      val of_field_diffs_exn : Field_diff.t list -> t
  end

  type t =
    | Set_to_c of C1.t * C2.t
    | Set_to_d of
        { d1 : D1.t
        ; d2 : D2.t
        }
    | Diff_c of Ldiffable.Tuples.Tuple2.For_inlined_tuple.Diff.t
    | Diff_d of D_record.Diff.t

  ...
end
```
<!--END-->
where

```ocaml
type Ldiffable.Tuples.Tuple2.For_inlined_tuple.Diff.t = Ldiffable.Tuples.Tuple2.Diff.t
```

Notice that for inlined records the ppx generates a helper module which looks the same as a regular record
module with `[@@deriving ldiff]`, but can be calculated from / applied to `local_` values
of `derived_on`.

Similar helper modules exist for tuples in `Ldiffable.Tuples.TupleN.For_inlined_tuple`


# Referencing other types

Since diffs are by default nested, any type you reference must also derive `ldiff` or
otherwise implement `Ldiffable.S`

For example, if `Time_ns.Ofday.t` doesn't implement `Ldiffable.S`, then the following:

```ocaml
type t = Time_ns.Ofday.t * Time_ns.Ofday.t
[@@deriving ldiff]
```

Will give you an error like:

```ocaml
Error: Unbound module Time_ns.Ofday.Diff
```

Often it's enough to just add `[@@deriving ldiff]` to the referenced type.

But occasionally that doesn't quite work. This section covers some special cases and how
to handle them.


## Atomic diffs

Some types, e.g. any `Identifiable`, or indeed `Time_ns.Ofday.t`, are super simple, and
what we really want is to have `type Diff.t = t` and only return a diff if the two values
aren't equal. We call such diffs `atomic`.

This section explains how you can mark your types as atomically diffable

### [@ldiff.atomic]

You can annotate any part of your type to use atomic diffs using the `[@ldiff.atomic]`
attribute.

For instance, the error from the example above will go away if you write

```ocaml
type t =
  (Time_ns.Ofday.t [@ldiff.atomic]) * (Time_ns.Ofday.t [@ldiff.atomic])
[@@deriving ldiff]
```

The annotation also works for record fields:

```ocaml
type t =
  { start : Time_ns.Ofday.t [@ldiff.atomic]
  ; stop : Time_ns.Ofday.t [@ldiff.atomic]
  }
[@@deriving ldiff]
```

and for variants:

```ocaml
type t =
  | Start of Time_ns.Ofday.t [@ldiff.atomic]
  | Stop of Time_ns.Ofday.t [@ldiff.atomic]
[@@deriving ldiff]
```

The two examples above are in fact equivalent to

```ocaml
type t =
  { start : (Time_ns.Ofday.t [@ldiff.atomic])
  ; stop : (Time_ns.Ofday.t [@ldiff.atomic])
  }
  [@@deriving ldiff]
```
and

```ocaml
type t =
  | Start of (Time_ns.Ofday.t [@ldiff.atomic])
  | Stop of (Time_ns.Ofday.t [@ldiff.atomic])
  [@@deriving ldiff]
```

but slightly nicer to write.

### ~how:"atomic"

Finally, you can mark your whole type atomic by using the `how` parameter:

```ocaml
type t = Time_ns.Ofday.t [@@deriving equal, ldiff ~how:"atomic"]
```

Any type that you mark `atomic`, must also derive / otherwise implement `equal`.


### Atomic types
To avoid having to add the annotations everywhere, you can also make the referenced type
implement `Ldiffable.S` in an atomic fashion.

To do this, use `Ldiffable.Make_atomic`, e.g.

```ocaml
module Id : sig
  include Identifiable
  include Ldiffable.S with type t := t and type Diff.t = t
end = struct
  include String
  include functor Ldiffable.Atomic.Make
end
```

Note that `Ldiffable.Atomic.Make` requires that your type derives `equal`, `sexp` and `bin_io`.

If you don't derive all of those (but do derive, say, `sexp_of`), `Ldiffable.Atomic` will
still work but you will need to be more verbose, e.g.

```ocaml
module Id : sig
  type t [@@deriving sexp_of]

  module Diff : sig
    type derived_on = t [@@deriving sexp_of]
    type t = derived_on [@@deriving sexp_of]

    include Ldiffable.Diff.S_plain with type t := t and type derived_on := derived_on
  end
end = struct
  type t = some_type [@@deriving equal, sexp_of]

  module Diff = struct
    type derived_on = t [@@deriving equal, sexp_of]
    type t = derived_on [@@deriving equal, sexp_of]

    include functor Ldiffable.Atomic.Make_diff_plain
  end
end
```

### Atomic using compare

If a type doesn't implement `equal`, but does implement `compare`, you can also use the
`atomic_using_compare` attribute, which works in exactly the same way as `atomic`.

E.g. the following will work:

```ocaml
type t = Time_ns.Ofday.t [@@deriving compare, ldiff ~how:"atomic_using_compare"]
```

```ocaml
type t =
  { start : Time_ns.Ofday.t [@ldiff.atomic_using_compare]
  ; stop : Time_ns.Ofday.t [@ldiff.atomic_using_compare]
  }
[@@deriving ldiff]
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
2. If using `Ldiffable.Atomic.Make` add an override `let equal = [%compare.equal]`

The primitive `float` type is already handled using `compare`


## Sets and maps

Since diffs are nested, the following:

```ocaml
type t = My_id.Set.t [@@deriving ldiff]
```

may give you an error like

```ocaml
Error: Unbound module My_id.Set.Diff
```

and the following:

```ocaml
type t = My_data.t My_id.Map.t [@@deriving ldiff]
```

may give you an error like

```ocaml
Error: Unbound module My_id.Map.Diff
```

### ~how:"set" and ~how:"map"


Of course you could annotate the type with `atomic`, but you can also do better.

The `set` and `map` annotations will cause the diff to only contain "what changed", i.e.

```ocaml
type t = My_id.Set.t [@@deriving ldiff ~how:"set"]
```

will generate

```ocaml
module Diff : sig
  type derived_on = t
  type t = My_id.Set.Elt.t Ldiffable.Set_diff.t
  ...
end
```

where

```ocaml
module Ldiffable.Set_diff : sig
  module Change : sig
    type 'a t = | Add of 'a | Remove of 'a
  end

  type 'a t = 'a Change.t list
end
```

and

```ocaml
type t = My_data.t My_id.Map.t [@@deriving ldiff ~how:"map"]
```

will generate

```ocaml
module Diff : sig
  type derived_on = t
  type t = (My_id.Map.Key.t, My_data.t, My_data.Diff.t) Ldiffable.Map_diff.t
  ...
end
```

where

```ocaml
module Ldiffable.Map_diff : sig
  module Change : sig
    type ('key, 'a, 'a_diff) t =
      | Remove of 'key
      | Add of 'key * 'a
      | Diff of 'key * 'a_diff
    [@@deriving sexp, bin_io]
  end

  type ('key, 'a, 'a_diff) t = ('key, 'a, 'a_diff) Change.t list [@@deriving sexp, bin_io]
end
```

### Key and Elt

The above signatures mean we assume that `Set` has an `Elt` submodule, and `Map`
has a `Key` submodule. However, if you want to use a different key/elt, this can be
overridden. The overrides are particularly helpful for applicative functors.

E.g. for maps you can write

```ocaml
type t = float Map.Stable.V1.M(Int).t [@@deriving ldiff ~how:"map" ~key:int]
```

and for sets you can write

```ocaml
type t = Set.Stable.V1.M(Int).t [@@deriving ldiff ~how:"map" ~elt:int]
```

### [@ldiff.set] and [@ldiff.map]

The annotations work as attributes as well, in the same way that the `atomic` ones do.
This means all of the following will work:

```ocaml
type t = (My_id.Set.t [@ldiff.set]) * int [@@deriving ldiff]
```

```ocaml
type t = (My_data.t My_id.Map.t [@ldiff.map]) * int [@@deriving ldiff]
```

```ocaml
type t =
  { foo : My_id.Set.t [@ldiff.set]
  ; bar : int
  }
[@@deriving ldiff]
```

```ocaml
type t =
  { foo : My_data.t My_id.Map.t [@ldiff.map]
  ; bar : int
  }
[@@deriving ldiff]
```

```ocaml
type t =
  | Foo of My_id.Set.t [@ldiff.set]
  | Bar
[@@deriving ldiff]
```

```ocaml
type t =
  | Foo of My_data.t My_id.Map.t [@ldiff.map]
  | Bar
[@@deriving ldiff]
```


You can also override the key/elt using the attributes.

For maps you can write:
```ocaml
type t = (float Map.Stable.V1.M(Int).t [@ldiff.map (key : int)]) * int [@@deriving ldiff]
```

and for sets you can write:
```ocaml
type t = (Set.Stable.V1.M(Int).t [@@ldiff.set (elt : int)]) * int [@@deriving ldiff]
```

### Map value diffs

By default the values in a map are also diffed.

So if the following:

```ocaml
type t = Time_ns.Ofday.t My_id.Map.t [ldiff.map]
```

gives you an error

```ocaml
Error: Unbound module Time_ns.Ofday.Diff
```

you can fix it by writing


```ocaml
type t = (Time_ns.Ofday.t [@ldiff.atomic]) My_id.Map.t [ldiff.map]
```

### Ldiffable.Comparable.Make 

To avoid having to annotate the type with `set` and `map` everywhere, you can also add the following to `My_id`

```ocaml
module My_id : sig
  include Core.Identifiable

  include
    Ldiffable.Comparable.S
    with type t := t
     and type comparator_witness := comparator_witness
end = struct
  include Core.String
  include functor Ldiffable.Comparable.Make
end
```


"Set" and "map" diffs do not work on types like

```ocaml
type t = (elt, comparator_witness) Set.t
```

or

```
type t = (key, value, comparator_witness) Map.t
```

You will instead need to define helper modules

```ocaml
module Set_helper = struct
  type t = (elt, comparator_witness) Set.t
  module Elt = struct
    type t = elt
  end
end

module Map_helper = struct
  type t = (key, value, comparator_witness) Map.t
  module Key = struct
    type t = key
  end
end
```

# Abstract diffs

One problem that arises from using `atomic`, `atomic_using_compare`, `set` and `map` attributes is that
your type definition may look a bit polluted, since you need to use the attributes in both
the mli and the ml

E.g. the following is rather verbose

```ocaml
module Range : sig
  type t =
    { start : Time_ns.Ofday.t [@ldiff.atomic]
    ; stop : Time_ns.Ofday.t [@ldiff.atomic]
    }
  [@@deriving ldiff]
end = struct
  type t =
    { start : Time_ns.Ofday.t [@ldiff.atomic]
    ; stop : Time_ns.Ofday.t [@ldiff.atomic]
    }
  [@@deriving ldiff]
end
```

If you don't care about exposing what the diff type actually is, you can make the diff
abstract.

You can do this either directly by writing:

```ocaml
module Range : sig
  type t =
    { start : Time_ns.Ofday.t
    ; stop : Time_ns.Ofday.t
    }

  include Ldiffable.S with type t := t
end
```

or, equivalently, by using `~how:"abstract"`:

```ocaml
module Range : sig
  type t =
    { start : Time_ns.Ofday.t
    ; stop : Time_ns.Ofday.t
    }
  [@@deriving ldiff ~how:"abstract"]
end
```

# Stability

Want to use `[@@deriving ldiff]` with stable types and guarantee that `[Diff.t]` is also stable?
You can use the `[stable_version]` annotation, e.g.

```ocaml
type t = ... [@@deriving ldiff ~stable_version:1]
```

The resulting diff type won't change as long as it only references other types that are
both stable themselves and use the ldiff `[stable_version]` annotation (or atomic diffs).
Note that the compiler does NOT check that you got this right, so we suggest adding a bin
digest test for peace of mind.
 
Only version `1` exists at the moment. There are also currently no plans to change any the
types generated by ldiff, but the `stable_version` annotation will protect you in case we
ever do choose to make changes.


# Polling state RPC

`ppx_ldiff` can be used with `Polling_state_rpc` by calling `Ldiffable_polling_state_rpc_response.Polling_state_rpc_response.Make`

For example, the following will work:

```ocaml
module Query = Unit

module Response = struct
  type t =
    { foo : int
    ; bar : float
    ; baz : char
    } [@@deriving sexp, bin_io, ldiff]
end

let polling_state_rpc = 
    Polling_state_rpc.create
      ~name:"get-response"
      ~version:0
      ~query_equal:Query.equal
      ~bin_query:Query.bin_t
      (module Ldiffable_polling_state_rpc_response.Polling_state_rpc_response.Make
           (Response))
```

