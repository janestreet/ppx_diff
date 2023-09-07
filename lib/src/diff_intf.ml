module type S_plain = sig
  type derived_on
  type t

  val get : from:derived_on -> to_:derived_on -> t Optional_diff.t
  val apply_exn : derived_on -> t -> derived_on
end

module type S = sig
  type derived_on
  type t [@@deriving sexp, bin_io]

  include S_plain with type derived_on := derived_on and type t := t
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
end

module type S1 = sig
  type 'a derived_on
  type ('a, 'a_diff) t [@@deriving sexp, bin_io]

  include
    S1_plain
      with type 'a derived_on := 'a derived_on
       and type ('a, 'a_diff) t := ('a, 'a_diff) t
end
