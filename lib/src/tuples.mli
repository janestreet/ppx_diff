val max_supported : int * Lexing.position

(*$
  open! Core
  open Diffable_cinaps

  let () = print_string (Tuple_helpers.tuples_mli ())
*)
module Tuple2 : sig
  type ('a1, 'a2) t = 'a1 * 'a2 [@@deriving sexp, bin_io]

  module Diff : sig
    type ('a1, 'a2) derived_on = ('a1, 'a2) t

    module Entry_diff : sig
      type ('a1, 'a2, 'a1_diff, 'a2_diff) t =
        | T1 of 'a1_diff
        | T2 of 'a2_diff
      [@@deriving variants, sexp, bin_io, quickcheck]
    end

    type ('a1, 'a2, 'a1_diff, 'a2_diff) t =
      private
      ('a1, 'a2, 'a1_diff, 'a2_diff) Entry_diff.t list
    [@@deriving sexp, bin_io, quickcheck]

    val get
      :  (from:'a1 -> to_:'a1 -> 'a1_diff Optional_diff.t)
      -> (from:'a2 -> to_:'a2 -> 'a2_diff Optional_diff.t)
      -> from:('a1, 'a2) derived_on
      -> to_:('a1, 'a2) derived_on
      -> ('a1, 'a2, 'a1_diff, 'a2_diff) t Optional_diff.t

    val apply_exn
      :  ('a1 -> 'a1_diff -> 'a1)
      -> ('a2 -> 'a2_diff -> 'a2)
      -> ('a1, 'a2) derived_on
      -> ('a1, 'a2, 'a1_diff, 'a2_diff) t
      -> ('a1, 'a2) derived_on

    val of_list_exn
      :  ('a1_diff list -> 'a1_diff Optional_diff.t)
      -> ('a1 -> 'a1_diff -> 'a1)
      -> ('a2_diff list -> 'a2_diff Optional_diff.t)
      -> ('a2 -> 'a2_diff -> 'a2)
      -> ('a1, 'a2, 'a1_diff, 'a2_diff) t list
      -> ('a1, 'a2, 'a1_diff, 'a2_diff) t Optional_diff.t

    val singleton
      :  ('a1, 'a2, 'a1_diff, 'a2_diff) Entry_diff.t
      -> ('a1, 'a2, 'a1_diff, 'a2_diff) t

    val create : ?t1:'a1_diff -> ?t2:'a2_diff -> unit -> ('a1, 'a2, 'a1_diff, 'a2_diff) t

    val create_of_variants
      :  t1:('a1_diff, ('a1, 'a2, 'a1_diff, 'a2_diff) Entry_diff.t) Of_variant.t
      -> t2:('a2_diff, ('a1, 'a2, 'a1_diff, 'a2_diff) Entry_diff.t) Of_variant.t
      -> ('a1, 'a2, 'a1_diff, 'a2_diff) t
  end

  module For_inlined_tuple : sig
    type ('a1, 'a2) t = 'a1 Gel.t * 'a2 Gel.t [@@deriving sexp, bin_io]

    module Diff : sig
      type ('a1, 'a2) derived_on = ('a1, 'a2) t

      type ('a1, 'a2, 'a1_diff, 'a2_diff) t = ('a1, 'a2, 'a1_diff, 'a2_diff) Diff.t
      [@@deriving sexp, bin_io, quickcheck]

      val get
        :  (from:'a1 -> to_:'a1 -> 'a1_diff Optional_diff.t)
        -> (from:'a2 -> to_:'a2 -> 'a2_diff Optional_diff.t)
        -> from:('a1, 'a2) derived_on
        -> to_:('a1, 'a2) derived_on
        -> ('a1, 'a2, 'a1_diff, 'a2_diff) t Optional_diff.t

      val apply_exn
        :  ('a1 -> 'a1_diff -> 'a1)
        -> ('a2 -> 'a2_diff -> 'a2)
        -> ('a1, 'a2) derived_on
        -> ('a1, 'a2, 'a1_diff, 'a2_diff) t
        -> ('a1, 'a2) derived_on

      val of_list_exn
        :  ('a1_diff list -> 'a1_diff Optional_diff.t)
        -> ('a1 -> 'a1_diff -> 'a1)
        -> ('a2_diff list -> 'a2_diff Optional_diff.t)
        -> ('a2 -> 'a2_diff -> 'a2)
        -> ('a1, 'a2, 'a1_diff, 'a2_diff) t list
        -> ('a1, 'a2, 'a1_diff, 'a2_diff) t Optional_diff.t
    end
  end
end

module Tuple3 : sig
  type ('a1, 'a2, 'a3) t = 'a1 * 'a2 * 'a3 [@@deriving sexp, bin_io]

  module Diff : sig
    type ('a1, 'a2, 'a3) derived_on = ('a1, 'a2, 'a3) t

    module Entry_diff : sig
      type ('a1, 'a2, 'a3, 'a1_diff, 'a2_diff, 'a3_diff) t =
        | T1 of 'a1_diff
        | T2 of 'a2_diff
        | T3 of 'a3_diff
      [@@deriving variants, sexp, bin_io, quickcheck]
    end

    type ('a1, 'a2, 'a3, 'a1_diff, 'a2_diff, 'a3_diff) t =
      private
      ('a1, 'a2, 'a3, 'a1_diff, 'a2_diff, 'a3_diff) Entry_diff.t list
    [@@deriving sexp, bin_io, quickcheck]

    val get
      :  (from:'a1 -> to_:'a1 -> 'a1_diff Optional_diff.t)
      -> (from:'a2 -> to_:'a2 -> 'a2_diff Optional_diff.t)
      -> (from:'a3 -> to_:'a3 -> 'a3_diff Optional_diff.t)
      -> from:('a1, 'a2, 'a3) derived_on
      -> to_:('a1, 'a2, 'a3) derived_on
      -> ('a1, 'a2, 'a3, 'a1_diff, 'a2_diff, 'a3_diff) t Optional_diff.t

    val apply_exn
      :  ('a1 -> 'a1_diff -> 'a1)
      -> ('a2 -> 'a2_diff -> 'a2)
      -> ('a3 -> 'a3_diff -> 'a3)
      -> ('a1, 'a2, 'a3) derived_on
      -> ('a1, 'a2, 'a3, 'a1_diff, 'a2_diff, 'a3_diff) t
      -> ('a1, 'a2, 'a3) derived_on

    val of_list_exn
      :  ('a1_diff list -> 'a1_diff Optional_diff.t)
      -> ('a1 -> 'a1_diff -> 'a1)
      -> ('a2_diff list -> 'a2_diff Optional_diff.t)
      -> ('a2 -> 'a2_diff -> 'a2)
      -> ('a3_diff list -> 'a3_diff Optional_diff.t)
      -> ('a3 -> 'a3_diff -> 'a3)
      -> ('a1, 'a2, 'a3, 'a1_diff, 'a2_diff, 'a3_diff) t list
      -> ('a1, 'a2, 'a3, 'a1_diff, 'a2_diff, 'a3_diff) t Optional_diff.t

    val singleton
      :  ('a1, 'a2, 'a3, 'a1_diff, 'a2_diff, 'a3_diff) Entry_diff.t
      -> ('a1, 'a2, 'a3, 'a1_diff, 'a2_diff, 'a3_diff) t

    val create
      :  ?t1:'a1_diff
      -> ?t2:'a2_diff
      -> ?t3:'a3_diff
      -> unit
      -> ('a1, 'a2, 'a3, 'a1_diff, 'a2_diff, 'a3_diff) t

    val create_of_variants
      :  t1:
           ( 'a1_diff
           , ('a1, 'a2, 'a3, 'a1_diff, 'a2_diff, 'a3_diff) Entry_diff.t )
           Of_variant.t
      -> t2:
           ( 'a2_diff
           , ('a1, 'a2, 'a3, 'a1_diff, 'a2_diff, 'a3_diff) Entry_diff.t )
           Of_variant.t
      -> t3:
           ( 'a3_diff
           , ('a1, 'a2, 'a3, 'a1_diff, 'a2_diff, 'a3_diff) Entry_diff.t )
           Of_variant.t
      -> ('a1, 'a2, 'a3, 'a1_diff, 'a2_diff, 'a3_diff) t
  end

  module For_inlined_tuple : sig
    type ('a1, 'a2, 'a3) t = 'a1 Gel.t * 'a2 Gel.t * 'a3 Gel.t [@@deriving sexp, bin_io]

    module Diff : sig
      type ('a1, 'a2, 'a3) derived_on = ('a1, 'a2, 'a3) t

      type ('a1, 'a2, 'a3, 'a1_diff, 'a2_diff, 'a3_diff) t =
        ('a1, 'a2, 'a3, 'a1_diff, 'a2_diff, 'a3_diff) Diff.t
      [@@deriving sexp, bin_io, quickcheck]

      val get
        :  (from:'a1 -> to_:'a1 -> 'a1_diff Optional_diff.t)
        -> (from:'a2 -> to_:'a2 -> 'a2_diff Optional_diff.t)
        -> (from:'a3 -> to_:'a3 -> 'a3_diff Optional_diff.t)
        -> from:('a1, 'a2, 'a3) derived_on
        -> to_:('a1, 'a2, 'a3) derived_on
        -> ('a1, 'a2, 'a3, 'a1_diff, 'a2_diff, 'a3_diff) t Optional_diff.t

      val apply_exn
        :  ('a1 -> 'a1_diff -> 'a1)
        -> ('a2 -> 'a2_diff -> 'a2)
        -> ('a3 -> 'a3_diff -> 'a3)
        -> ('a1, 'a2, 'a3) derived_on
        -> ('a1, 'a2, 'a3, 'a1_diff, 'a2_diff, 'a3_diff) t
        -> ('a1, 'a2, 'a3) derived_on

      val of_list_exn
        :  ('a1_diff list -> 'a1_diff Optional_diff.t)
        -> ('a1 -> 'a1_diff -> 'a1)
        -> ('a2_diff list -> 'a2_diff Optional_diff.t)
        -> ('a2 -> 'a2_diff -> 'a2)
        -> ('a3_diff list -> 'a3_diff Optional_diff.t)
        -> ('a3 -> 'a3_diff -> 'a3)
        -> ('a1, 'a2, 'a3, 'a1_diff, 'a2_diff, 'a3_diff) t list
        -> ('a1, 'a2, 'a3, 'a1_diff, 'a2_diff, 'a3_diff) t Optional_diff.t
    end
  end
end

module Tuple4 : sig
  type ('a1, 'a2, 'a3, 'a4) t = 'a1 * 'a2 * 'a3 * 'a4 [@@deriving sexp, bin_io]

  module Diff : sig
    type ('a1, 'a2, 'a3, 'a4) derived_on = ('a1, 'a2, 'a3, 'a4) t

    module Entry_diff : sig
      type ('a1, 'a2, 'a3, 'a4, 'a1_diff, 'a2_diff, 'a3_diff, 'a4_diff) t =
        | T1 of 'a1_diff
        | T2 of 'a2_diff
        | T3 of 'a3_diff
        | T4 of 'a4_diff
      [@@deriving variants, sexp, bin_io, quickcheck]
    end

    type ('a1, 'a2, 'a3, 'a4, 'a1_diff, 'a2_diff, 'a3_diff, 'a4_diff) t =
      private
      ('a1, 'a2, 'a3, 'a4, 'a1_diff, 'a2_diff, 'a3_diff, 'a4_diff) Entry_diff.t list
    [@@deriving sexp, bin_io, quickcheck]

    val get
      :  (from:'a1 -> to_:'a1 -> 'a1_diff Optional_diff.t)
      -> (from:'a2 -> to_:'a2 -> 'a2_diff Optional_diff.t)
      -> (from:'a3 -> to_:'a3 -> 'a3_diff Optional_diff.t)
      -> (from:'a4 -> to_:'a4 -> 'a4_diff Optional_diff.t)
      -> from:('a1, 'a2, 'a3, 'a4) derived_on
      -> to_:('a1, 'a2, 'a3, 'a4) derived_on
      -> ('a1, 'a2, 'a3, 'a4, 'a1_diff, 'a2_diff, 'a3_diff, 'a4_diff) t Optional_diff.t

    val apply_exn
      :  ('a1 -> 'a1_diff -> 'a1)
      -> ('a2 -> 'a2_diff -> 'a2)
      -> ('a3 -> 'a3_diff -> 'a3)
      -> ('a4 -> 'a4_diff -> 'a4)
      -> ('a1, 'a2, 'a3, 'a4) derived_on
      -> ('a1, 'a2, 'a3, 'a4, 'a1_diff, 'a2_diff, 'a3_diff, 'a4_diff) t
      -> ('a1, 'a2, 'a3, 'a4) derived_on

    val of_list_exn
      :  ('a1_diff list -> 'a1_diff Optional_diff.t)
      -> ('a1 -> 'a1_diff -> 'a1)
      -> ('a2_diff list -> 'a2_diff Optional_diff.t)
      -> ('a2 -> 'a2_diff -> 'a2)
      -> ('a3_diff list -> 'a3_diff Optional_diff.t)
      -> ('a3 -> 'a3_diff -> 'a3)
      -> ('a4_diff list -> 'a4_diff Optional_diff.t)
      -> ('a4 -> 'a4_diff -> 'a4)
      -> ('a1, 'a2, 'a3, 'a4, 'a1_diff, 'a2_diff, 'a3_diff, 'a4_diff) t list
      -> ('a1, 'a2, 'a3, 'a4, 'a1_diff, 'a2_diff, 'a3_diff, 'a4_diff) t Optional_diff.t

    val singleton
      :  ('a1, 'a2, 'a3, 'a4, 'a1_diff, 'a2_diff, 'a3_diff, 'a4_diff) Entry_diff.t
      -> ('a1, 'a2, 'a3, 'a4, 'a1_diff, 'a2_diff, 'a3_diff, 'a4_diff) t

    val create
      :  ?t1:'a1_diff
      -> ?t2:'a2_diff
      -> ?t3:'a3_diff
      -> ?t4:'a4_diff
      -> unit
      -> ('a1, 'a2, 'a3, 'a4, 'a1_diff, 'a2_diff, 'a3_diff, 'a4_diff) t

    val create_of_variants
      :  t1:
           ( 'a1_diff
           , ('a1, 'a2, 'a3, 'a4, 'a1_diff, 'a2_diff, 'a3_diff, 'a4_diff) Entry_diff.t )
           Of_variant.t
      -> t2:
           ( 'a2_diff
           , ('a1, 'a2, 'a3, 'a4, 'a1_diff, 'a2_diff, 'a3_diff, 'a4_diff) Entry_diff.t )
           Of_variant.t
      -> t3:
           ( 'a3_diff
           , ('a1, 'a2, 'a3, 'a4, 'a1_diff, 'a2_diff, 'a3_diff, 'a4_diff) Entry_diff.t )
           Of_variant.t
      -> t4:
           ( 'a4_diff
           , ('a1, 'a2, 'a3, 'a4, 'a1_diff, 'a2_diff, 'a3_diff, 'a4_diff) Entry_diff.t )
           Of_variant.t
      -> ('a1, 'a2, 'a3, 'a4, 'a1_diff, 'a2_diff, 'a3_diff, 'a4_diff) t
  end

  module For_inlined_tuple : sig
    type ('a1, 'a2, 'a3, 'a4) t = 'a1 Gel.t * 'a2 Gel.t * 'a3 Gel.t * 'a4 Gel.t
    [@@deriving sexp, bin_io]

    module Diff : sig
      type ('a1, 'a2, 'a3, 'a4) derived_on = ('a1, 'a2, 'a3, 'a4) t

      type ('a1, 'a2, 'a3, 'a4, 'a1_diff, 'a2_diff, 'a3_diff, 'a4_diff) t =
        ('a1, 'a2, 'a3, 'a4, 'a1_diff, 'a2_diff, 'a3_diff, 'a4_diff) Diff.t
      [@@deriving sexp, bin_io, quickcheck]

      val get
        :  (from:'a1 -> to_:'a1 -> 'a1_diff Optional_diff.t)
        -> (from:'a2 -> to_:'a2 -> 'a2_diff Optional_diff.t)
        -> (from:'a3 -> to_:'a3 -> 'a3_diff Optional_diff.t)
        -> (from:'a4 -> to_:'a4 -> 'a4_diff Optional_diff.t)
        -> from:('a1, 'a2, 'a3, 'a4) derived_on
        -> to_:('a1, 'a2, 'a3, 'a4) derived_on
        -> ('a1, 'a2, 'a3, 'a4, 'a1_diff, 'a2_diff, 'a3_diff, 'a4_diff) t Optional_diff.t

      val apply_exn
        :  ('a1 -> 'a1_diff -> 'a1)
        -> ('a2 -> 'a2_diff -> 'a2)
        -> ('a3 -> 'a3_diff -> 'a3)
        -> ('a4 -> 'a4_diff -> 'a4)
        -> ('a1, 'a2, 'a3, 'a4) derived_on
        -> ('a1, 'a2, 'a3, 'a4, 'a1_diff, 'a2_diff, 'a3_diff, 'a4_diff) t
        -> ('a1, 'a2, 'a3, 'a4) derived_on

      val of_list_exn
        :  ('a1_diff list -> 'a1_diff Optional_diff.t)
        -> ('a1 -> 'a1_diff -> 'a1)
        -> ('a2_diff list -> 'a2_diff Optional_diff.t)
        -> ('a2 -> 'a2_diff -> 'a2)
        -> ('a3_diff list -> 'a3_diff Optional_diff.t)
        -> ('a3 -> 'a3_diff -> 'a3)
        -> ('a4_diff list -> 'a4_diff Optional_diff.t)
        -> ('a4 -> 'a4_diff -> 'a4)
        -> ('a1, 'a2, 'a3, 'a4, 'a1_diff, 'a2_diff, 'a3_diff, 'a4_diff) t list
        -> ('a1, 'a2, 'a3, 'a4, 'a1_diff, 'a2_diff, 'a3_diff, 'a4_diff) t Optional_diff.t
    end
  end
end

module Tuple5 : sig
  type ('a1, 'a2, 'a3, 'a4, 'a5) t = 'a1 * 'a2 * 'a3 * 'a4 * 'a5 [@@deriving sexp, bin_io]

  module Diff : sig
    type ('a1, 'a2, 'a3, 'a4, 'a5) derived_on = ('a1, 'a2, 'a3, 'a4, 'a5) t

    module Entry_diff : sig
      type ('a1, 'a2, 'a3, 'a4, 'a5, 'a1_diff, 'a2_diff, 'a3_diff, 'a4_diff, 'a5_diff) t =
        | T1 of 'a1_diff
        | T2 of 'a2_diff
        | T3 of 'a3_diff
        | T4 of 'a4_diff
        | T5 of 'a5_diff
      [@@deriving variants, sexp, bin_io, quickcheck]
    end

    type ('a1, 'a2, 'a3, 'a4, 'a5, 'a1_diff, 'a2_diff, 'a3_diff, 'a4_diff, 'a5_diff) t =
      private
      ( 'a1
      , 'a2
      , 'a3
      , 'a4
      , 'a5
      , 'a1_diff
      , 'a2_diff
      , 'a3_diff
      , 'a4_diff
      , 'a5_diff )
      Entry_diff.t
      list
    [@@deriving sexp, bin_io, quickcheck]

    val get
      :  (from:'a1 -> to_:'a1 -> 'a1_diff Optional_diff.t)
      -> (from:'a2 -> to_:'a2 -> 'a2_diff Optional_diff.t)
      -> (from:'a3 -> to_:'a3 -> 'a3_diff Optional_diff.t)
      -> (from:'a4 -> to_:'a4 -> 'a4_diff Optional_diff.t)
      -> (from:'a5 -> to_:'a5 -> 'a5_diff Optional_diff.t)
      -> from:('a1, 'a2, 'a3, 'a4, 'a5) derived_on
      -> to_:('a1, 'a2, 'a3, 'a4, 'a5) derived_on
      -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a1_diff, 'a2_diff, 'a3_diff, 'a4_diff, 'a5_diff) t
         Optional_diff.t

    val apply_exn
      :  ('a1 -> 'a1_diff -> 'a1)
      -> ('a2 -> 'a2_diff -> 'a2)
      -> ('a3 -> 'a3_diff -> 'a3)
      -> ('a4 -> 'a4_diff -> 'a4)
      -> ('a5 -> 'a5_diff -> 'a5)
      -> ('a1, 'a2, 'a3, 'a4, 'a5) derived_on
      -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a1_diff, 'a2_diff, 'a3_diff, 'a4_diff, 'a5_diff) t
      -> ('a1, 'a2, 'a3, 'a4, 'a5) derived_on

    val of_list_exn
      :  ('a1_diff list -> 'a1_diff Optional_diff.t)
      -> ('a1 -> 'a1_diff -> 'a1)
      -> ('a2_diff list -> 'a2_diff Optional_diff.t)
      -> ('a2 -> 'a2_diff -> 'a2)
      -> ('a3_diff list -> 'a3_diff Optional_diff.t)
      -> ('a3 -> 'a3_diff -> 'a3)
      -> ('a4_diff list -> 'a4_diff Optional_diff.t)
      -> ('a4 -> 'a4_diff -> 'a4)
      -> ('a5_diff list -> 'a5_diff Optional_diff.t)
      -> ('a5 -> 'a5_diff -> 'a5)
      -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a1_diff, 'a2_diff, 'a3_diff, 'a4_diff, 'a5_diff) t
         list
      -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a1_diff, 'a2_diff, 'a3_diff, 'a4_diff, 'a5_diff) t
         Optional_diff.t

    val singleton
      :  ( 'a1
         , 'a2
         , 'a3
         , 'a4
         , 'a5
         , 'a1_diff
         , 'a2_diff
         , 'a3_diff
         , 'a4_diff
         , 'a5_diff )
         Entry_diff.t
      -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a1_diff, 'a2_diff, 'a3_diff, 'a4_diff, 'a5_diff) t

    val create
      :  ?t1:'a1_diff
      -> ?t2:'a2_diff
      -> ?t3:'a3_diff
      -> ?t4:'a4_diff
      -> ?t5:'a5_diff
      -> unit
      -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a1_diff, 'a2_diff, 'a3_diff, 'a4_diff, 'a5_diff) t

    val create_of_variants
      :  t1:
           ( 'a1_diff
           , ( 'a1
             , 'a2
             , 'a3
             , 'a4
             , 'a5
             , 'a1_diff
             , 'a2_diff
             , 'a3_diff
             , 'a4_diff
             , 'a5_diff )
             Entry_diff.t )
           Of_variant.t
      -> t2:
           ( 'a2_diff
           , ( 'a1
             , 'a2
             , 'a3
             , 'a4
             , 'a5
             , 'a1_diff
             , 'a2_diff
             , 'a3_diff
             , 'a4_diff
             , 'a5_diff )
             Entry_diff.t )
           Of_variant.t
      -> t3:
           ( 'a3_diff
           , ( 'a1
             , 'a2
             , 'a3
             , 'a4
             , 'a5
             , 'a1_diff
             , 'a2_diff
             , 'a3_diff
             , 'a4_diff
             , 'a5_diff )
             Entry_diff.t )
           Of_variant.t
      -> t4:
           ( 'a4_diff
           , ( 'a1
             , 'a2
             , 'a3
             , 'a4
             , 'a5
             , 'a1_diff
             , 'a2_diff
             , 'a3_diff
             , 'a4_diff
             , 'a5_diff )
             Entry_diff.t )
           Of_variant.t
      -> t5:
           ( 'a5_diff
           , ( 'a1
             , 'a2
             , 'a3
             , 'a4
             , 'a5
             , 'a1_diff
             , 'a2_diff
             , 'a3_diff
             , 'a4_diff
             , 'a5_diff )
             Entry_diff.t )
           Of_variant.t
      -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a1_diff, 'a2_diff, 'a3_diff, 'a4_diff, 'a5_diff) t
  end

  module For_inlined_tuple : sig
    type ('a1, 'a2, 'a3, 'a4, 'a5) t =
      'a1 Gel.t * 'a2 Gel.t * 'a3 Gel.t * 'a4 Gel.t * 'a5 Gel.t
    [@@deriving sexp, bin_io]

    module Diff : sig
      type ('a1, 'a2, 'a3, 'a4, 'a5) derived_on = ('a1, 'a2, 'a3, 'a4, 'a5) t

      type ('a1, 'a2, 'a3, 'a4, 'a5, 'a1_diff, 'a2_diff, 'a3_diff, 'a4_diff, 'a5_diff) t =
        ('a1, 'a2, 'a3, 'a4, 'a5, 'a1_diff, 'a2_diff, 'a3_diff, 'a4_diff, 'a5_diff) Diff.t
      [@@deriving sexp, bin_io, quickcheck]

      val get
        :  (from:'a1 -> to_:'a1 -> 'a1_diff Optional_diff.t)
        -> (from:'a2 -> to_:'a2 -> 'a2_diff Optional_diff.t)
        -> (from:'a3 -> to_:'a3 -> 'a3_diff Optional_diff.t)
        -> (from:'a4 -> to_:'a4 -> 'a4_diff Optional_diff.t)
        -> (from:'a5 -> to_:'a5 -> 'a5_diff Optional_diff.t)
        -> from:('a1, 'a2, 'a3, 'a4, 'a5) derived_on
        -> to_:('a1, 'a2, 'a3, 'a4, 'a5) derived_on
        -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a1_diff, 'a2_diff, 'a3_diff, 'a4_diff, 'a5_diff) t
           Optional_diff.t

      val apply_exn
        :  ('a1 -> 'a1_diff -> 'a1)
        -> ('a2 -> 'a2_diff -> 'a2)
        -> ('a3 -> 'a3_diff -> 'a3)
        -> ('a4 -> 'a4_diff -> 'a4)
        -> ('a5 -> 'a5_diff -> 'a5)
        -> ('a1, 'a2, 'a3, 'a4, 'a5) derived_on
        -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a1_diff, 'a2_diff, 'a3_diff, 'a4_diff, 'a5_diff) t
        -> ('a1, 'a2, 'a3, 'a4, 'a5) derived_on

      val of_list_exn
        :  ('a1_diff list -> 'a1_diff Optional_diff.t)
        -> ('a1 -> 'a1_diff -> 'a1)
        -> ('a2_diff list -> 'a2_diff Optional_diff.t)
        -> ('a2 -> 'a2_diff -> 'a2)
        -> ('a3_diff list -> 'a3_diff Optional_diff.t)
        -> ('a3 -> 'a3_diff -> 'a3)
        -> ('a4_diff list -> 'a4_diff Optional_diff.t)
        -> ('a4 -> 'a4_diff -> 'a4)
        -> ('a5_diff list -> 'a5_diff Optional_diff.t)
        -> ('a5 -> 'a5_diff -> 'a5)
        -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a1_diff, 'a2_diff, 'a3_diff, 'a4_diff, 'a5_diff) t
           list
        -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a1_diff, 'a2_diff, 'a3_diff, 'a4_diff, 'a5_diff) t
           Optional_diff.t
    end
  end
end

module Tuple6 : sig
  type ('a1, 'a2, 'a3, 'a4, 'a5, 'a6) t = 'a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6
  [@@deriving sexp, bin_io]

  module Diff : sig
    type ('a1, 'a2, 'a3, 'a4, 'a5, 'a6) derived_on = ('a1, 'a2, 'a3, 'a4, 'a5, 'a6) t

    module Entry_diff : sig
      type ('a1
           , 'a2
           , 'a3
           , 'a4
           , 'a5
           , 'a6
           , 'a1_diff
           , 'a2_diff
           , 'a3_diff
           , 'a4_diff
           , 'a5_diff
           , 'a6_diff)
           t =
        | T1 of 'a1_diff
        | T2 of 'a2_diff
        | T3 of 'a3_diff
        | T4 of 'a4_diff
        | T5 of 'a5_diff
        | T6 of 'a6_diff
      [@@deriving variants, sexp, bin_io, quickcheck]
    end

    type ('a1
         , 'a2
         , 'a3
         , 'a4
         , 'a5
         , 'a6
         , 'a1_diff
         , 'a2_diff
         , 'a3_diff
         , 'a4_diff
         , 'a5_diff
         , 'a6_diff)
         t =
      private
      ( 'a1
      , 'a2
      , 'a3
      , 'a4
      , 'a5
      , 'a6
      , 'a1_diff
      , 'a2_diff
      , 'a3_diff
      , 'a4_diff
      , 'a5_diff
      , 'a6_diff )
      Entry_diff.t
      list
    [@@deriving sexp, bin_io, quickcheck]

    val get
      :  (from:'a1 -> to_:'a1 -> 'a1_diff Optional_diff.t)
      -> (from:'a2 -> to_:'a2 -> 'a2_diff Optional_diff.t)
      -> (from:'a3 -> to_:'a3 -> 'a3_diff Optional_diff.t)
      -> (from:'a4 -> to_:'a4 -> 'a4_diff Optional_diff.t)
      -> (from:'a5 -> to_:'a5 -> 'a5_diff Optional_diff.t)
      -> (from:'a6 -> to_:'a6 -> 'a6_diff Optional_diff.t)
      -> from:('a1, 'a2, 'a3, 'a4, 'a5, 'a6) derived_on
      -> to_:('a1, 'a2, 'a3, 'a4, 'a5, 'a6) derived_on
      -> ( 'a1
         , 'a2
         , 'a3
         , 'a4
         , 'a5
         , 'a6
         , 'a1_diff
         , 'a2_diff
         , 'a3_diff
         , 'a4_diff
         , 'a5_diff
         , 'a6_diff )
         t
         Optional_diff.t

    val apply_exn
      :  ('a1 -> 'a1_diff -> 'a1)
      -> ('a2 -> 'a2_diff -> 'a2)
      -> ('a3 -> 'a3_diff -> 'a3)
      -> ('a4 -> 'a4_diff -> 'a4)
      -> ('a5 -> 'a5_diff -> 'a5)
      -> ('a6 -> 'a6_diff -> 'a6)
      -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6) derived_on
      -> ( 'a1
         , 'a2
         , 'a3
         , 'a4
         , 'a5
         , 'a6
         , 'a1_diff
         , 'a2_diff
         , 'a3_diff
         , 'a4_diff
         , 'a5_diff
         , 'a6_diff )
         t
      -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6) derived_on

    val of_list_exn
      :  ('a1_diff list -> 'a1_diff Optional_diff.t)
      -> ('a1 -> 'a1_diff -> 'a1)
      -> ('a2_diff list -> 'a2_diff Optional_diff.t)
      -> ('a2 -> 'a2_diff -> 'a2)
      -> ('a3_diff list -> 'a3_diff Optional_diff.t)
      -> ('a3 -> 'a3_diff -> 'a3)
      -> ('a4_diff list -> 'a4_diff Optional_diff.t)
      -> ('a4 -> 'a4_diff -> 'a4)
      -> ('a5_diff list -> 'a5_diff Optional_diff.t)
      -> ('a5 -> 'a5_diff -> 'a5)
      -> ('a6_diff list -> 'a6_diff Optional_diff.t)
      -> ('a6 -> 'a6_diff -> 'a6)
      -> ( 'a1
         , 'a2
         , 'a3
         , 'a4
         , 'a5
         , 'a6
         , 'a1_diff
         , 'a2_diff
         , 'a3_diff
         , 'a4_diff
         , 'a5_diff
         , 'a6_diff )
         t
         list
      -> ( 'a1
         , 'a2
         , 'a3
         , 'a4
         , 'a5
         , 'a6
         , 'a1_diff
         , 'a2_diff
         , 'a3_diff
         , 'a4_diff
         , 'a5_diff
         , 'a6_diff )
         t
         Optional_diff.t

    val singleton
      :  ( 'a1
         , 'a2
         , 'a3
         , 'a4
         , 'a5
         , 'a6
         , 'a1_diff
         , 'a2_diff
         , 'a3_diff
         , 'a4_diff
         , 'a5_diff
         , 'a6_diff )
         Entry_diff.t
      -> ( 'a1
         , 'a2
         , 'a3
         , 'a4
         , 'a5
         , 'a6
         , 'a1_diff
         , 'a2_diff
         , 'a3_diff
         , 'a4_diff
         , 'a5_diff
         , 'a6_diff )
         t

    val create
      :  ?t1:'a1_diff
      -> ?t2:'a2_diff
      -> ?t3:'a3_diff
      -> ?t4:'a4_diff
      -> ?t5:'a5_diff
      -> ?t6:'a6_diff
      -> unit
      -> ( 'a1
         , 'a2
         , 'a3
         , 'a4
         , 'a5
         , 'a6
         , 'a1_diff
         , 'a2_diff
         , 'a3_diff
         , 'a4_diff
         , 'a5_diff
         , 'a6_diff )
         t

    val create_of_variants
      :  t1:
           ( 'a1_diff
           , ( 'a1
             , 'a2
             , 'a3
             , 'a4
             , 'a5
             , 'a6
             , 'a1_diff
             , 'a2_diff
             , 'a3_diff
             , 'a4_diff
             , 'a5_diff
             , 'a6_diff )
             Entry_diff.t )
           Of_variant.t
      -> t2:
           ( 'a2_diff
           , ( 'a1
             , 'a2
             , 'a3
             , 'a4
             , 'a5
             , 'a6
             , 'a1_diff
             , 'a2_diff
             , 'a3_diff
             , 'a4_diff
             , 'a5_diff
             , 'a6_diff )
             Entry_diff.t )
           Of_variant.t
      -> t3:
           ( 'a3_diff
           , ( 'a1
             , 'a2
             , 'a3
             , 'a4
             , 'a5
             , 'a6
             , 'a1_diff
             , 'a2_diff
             , 'a3_diff
             , 'a4_diff
             , 'a5_diff
             , 'a6_diff )
             Entry_diff.t )
           Of_variant.t
      -> t4:
           ( 'a4_diff
           , ( 'a1
             , 'a2
             , 'a3
             , 'a4
             , 'a5
             , 'a6
             , 'a1_diff
             , 'a2_diff
             , 'a3_diff
             , 'a4_diff
             , 'a5_diff
             , 'a6_diff )
             Entry_diff.t )
           Of_variant.t
      -> t5:
           ( 'a5_diff
           , ( 'a1
             , 'a2
             , 'a3
             , 'a4
             , 'a5
             , 'a6
             , 'a1_diff
             , 'a2_diff
             , 'a3_diff
             , 'a4_diff
             , 'a5_diff
             , 'a6_diff )
             Entry_diff.t )
           Of_variant.t
      -> t6:
           ( 'a6_diff
           , ( 'a1
             , 'a2
             , 'a3
             , 'a4
             , 'a5
             , 'a6
             , 'a1_diff
             , 'a2_diff
             , 'a3_diff
             , 'a4_diff
             , 'a5_diff
             , 'a6_diff )
             Entry_diff.t )
           Of_variant.t
      -> ( 'a1
         , 'a2
         , 'a3
         , 'a4
         , 'a5
         , 'a6
         , 'a1_diff
         , 'a2_diff
         , 'a3_diff
         , 'a4_diff
         , 'a5_diff
         , 'a6_diff )
         t
  end

  module For_inlined_tuple : sig
    type ('a1, 'a2, 'a3, 'a4, 'a5, 'a6) t =
      'a1 Gel.t * 'a2 Gel.t * 'a3 Gel.t * 'a4 Gel.t * 'a5 Gel.t * 'a6 Gel.t
    [@@deriving sexp, bin_io]

    module Diff : sig
      type ('a1, 'a2, 'a3, 'a4, 'a5, 'a6) derived_on = ('a1, 'a2, 'a3, 'a4, 'a5, 'a6) t

      type ('a1
           , 'a2
           , 'a3
           , 'a4
           , 'a5
           , 'a6
           , 'a1_diff
           , 'a2_diff
           , 'a3_diff
           , 'a4_diff
           , 'a5_diff
           , 'a6_diff)
           t =
        ( 'a1
        , 'a2
        , 'a3
        , 'a4
        , 'a5
        , 'a6
        , 'a1_diff
        , 'a2_diff
        , 'a3_diff
        , 'a4_diff
        , 'a5_diff
        , 'a6_diff )
        Diff.t
      [@@deriving sexp, bin_io, quickcheck]

      val get
        :  (from:'a1 -> to_:'a1 -> 'a1_diff Optional_diff.t)
        -> (from:'a2 -> to_:'a2 -> 'a2_diff Optional_diff.t)
        -> (from:'a3 -> to_:'a3 -> 'a3_diff Optional_diff.t)
        -> (from:'a4 -> to_:'a4 -> 'a4_diff Optional_diff.t)
        -> (from:'a5 -> to_:'a5 -> 'a5_diff Optional_diff.t)
        -> (from:'a6 -> to_:'a6 -> 'a6_diff Optional_diff.t)
        -> from:('a1, 'a2, 'a3, 'a4, 'a5, 'a6) derived_on
        -> to_:('a1, 'a2, 'a3, 'a4, 'a5, 'a6) derived_on
        -> ( 'a1
           , 'a2
           , 'a3
           , 'a4
           , 'a5
           , 'a6
           , 'a1_diff
           , 'a2_diff
           , 'a3_diff
           , 'a4_diff
           , 'a5_diff
           , 'a6_diff )
           t
           Optional_diff.t

      val apply_exn
        :  ('a1 -> 'a1_diff -> 'a1)
        -> ('a2 -> 'a2_diff -> 'a2)
        -> ('a3 -> 'a3_diff -> 'a3)
        -> ('a4 -> 'a4_diff -> 'a4)
        -> ('a5 -> 'a5_diff -> 'a5)
        -> ('a6 -> 'a6_diff -> 'a6)
        -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6) derived_on
        -> ( 'a1
           , 'a2
           , 'a3
           , 'a4
           , 'a5
           , 'a6
           , 'a1_diff
           , 'a2_diff
           , 'a3_diff
           , 'a4_diff
           , 'a5_diff
           , 'a6_diff )
           t
        -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6) derived_on

      val of_list_exn
        :  ('a1_diff list -> 'a1_diff Optional_diff.t)
        -> ('a1 -> 'a1_diff -> 'a1)
        -> ('a2_diff list -> 'a2_diff Optional_diff.t)
        -> ('a2 -> 'a2_diff -> 'a2)
        -> ('a3_diff list -> 'a3_diff Optional_diff.t)
        -> ('a3 -> 'a3_diff -> 'a3)
        -> ('a4_diff list -> 'a4_diff Optional_diff.t)
        -> ('a4 -> 'a4_diff -> 'a4)
        -> ('a5_diff list -> 'a5_diff Optional_diff.t)
        -> ('a5 -> 'a5_diff -> 'a5)
        -> ('a6_diff list -> 'a6_diff Optional_diff.t)
        -> ('a6 -> 'a6_diff -> 'a6)
        -> ( 'a1
           , 'a2
           , 'a3
           , 'a4
           , 'a5
           , 'a6
           , 'a1_diff
           , 'a2_diff
           , 'a3_diff
           , 'a4_diff
           , 'a5_diff
           , 'a6_diff )
           t
           list
        -> ( 'a1
           , 'a2
           , 'a3
           , 'a4
           , 'a5
           , 'a6
           , 'a1_diff
           , 'a2_diff
           , 'a3_diff
           , 'a4_diff
           , 'a5_diff
           , 'a6_diff )
           t
           Optional_diff.t
    end
  end
end
(*$*)
