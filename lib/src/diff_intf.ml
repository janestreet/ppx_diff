module type S_plain = sig
  type derived_on
  type t

  val get : from:derived_on -> to_:derived_on -> t Optional_diff.t
  val apply_exn : derived_on -> t -> derived_on
  val of_list_exn : t list -> t Optional_diff.t
end

module type S = sig
  type derived_on
  type t [@@deriving sexp, bin_io]

  include S_plain with type derived_on := derived_on and type t := t
end

module type S_atomic = sig
  type derived_on

  include S with type derived_on := derived_on and type t = derived_on
end

module type S1_plain = sig
  type 'a derived_on
  type ('a, 'a_diff) t

  val get
    :  (from:'a -> to_:'a -> 'a_diff Optional_diff.t)
    -> from:'a derived_on
    -> to_:'a derived_on
    -> ('a, 'a_diff) t Optional_diff.t

  val apply_exn
    :  ('a -> 'a_diff -> 'a)
    -> 'a derived_on
    -> ('a, 'a_diff) t
    -> 'a derived_on

  (* None if and only if the input list is empty *)
  val of_list_exn
    :  ('a_diff list -> 'a_diff Optional_diff.t)
    -> ('a -> 'a_diff -> 'a)
    -> ('a, 'a_diff) t list
    -> ('a, 'a_diff) t Optional_diff.t
end

module type S1 = sig
  type 'a derived_on
  type ('a, 'a_diff) t [@@deriving sexp, bin_io]

  include
    S1_plain
      with type 'a derived_on := 'a derived_on
       and type ('a, 'a_diff) t := ('a, 'a_diff) t
end

module type S2_plain = sig
  type ('a, 'b) derived_on
  type ('a, 'b, 'a_diff, 'b_diff) t

  val get
    :  (from:'a -> to_:'a -> 'a_diff Optional_diff.t)
    -> (from:'b -> to_:'b -> 'b_diff Optional_diff.t)
    -> from:('a, 'b) derived_on
    -> to_:('a, 'b) derived_on
    -> ('a, 'b, 'a_diff, 'b_diff) t Optional_diff.t

  val apply_exn
    :  ('a -> 'a_diff -> 'a)
    -> ('b -> 'b_diff -> 'b)
    -> ('a, 'b) derived_on
    -> ('a, 'b, 'a_diff, 'b_diff) t
    -> ('a, 'b) derived_on

  val of_list_exn
    :  ('a_diff list -> 'a_diff Optional_diff.t)
    -> ('a -> 'a_diff -> 'a)
    -> ('b_diff list -> 'b_diff Optional_diff.t)
    -> ('b -> 'b_diff -> 'b)
    -> ('a, 'b, 'a_diff, 'b_diff) t list
    -> ('a, 'b, 'a_diff, 'b_diff) t Optional_diff.t
end

module type S2 = sig
  type ('a, 'b) derived_on
  type ('a, 'b, 'a_diff, 'b_diff) t [@@deriving sexp, bin_io]

  include
    S2_plain
      with type ('a, 'b) derived_on := ('a, 'b) derived_on
       and type ('a, 'b, 'a_diff, 'b_diff) t := ('a, 'b, 'a_diff, 'b_diff) t
end
