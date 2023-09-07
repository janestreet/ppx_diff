open Core

(*$
  open! Core
  open Ldiffable_cinaps

  let () = print_string (Tuple_helpers.tuples_ml ())
*)
module Tuple2 = struct
  type ('a1, 'a2) t = 'a1 * 'a2 [@@deriving sexp, bin_io]

  module Diff = struct
    type ('a1, 'a2) derived_on = ('a1, 'a2) t

    module Entry_diff = struct
      type ('a1, 'a2, 'a1_diff, 'a2_diff) t =
        | T1 of 'a1_diff
        | T2 of 'a2_diff
      [@@deriving variants, sexp, bin_io]
    end

    open Entry_diff

    type ('a1, 'a2, 'a1_diff, 'a2_diff) t =
      ('a1, 'a2, 'a1_diff, 'a2_diff) Entry_diff.t list
    [@@deriving sexp, bin_io]

    let get get1 get2 ~from ~to_ =
      if Base.phys_equal from to_
      then Optional_diff.none
      else (
        let from_1, from_2 = from in
        let to_1, to_2 = to_ in
        let diff = [] in
        let diff =
          let d = get2 ~from:from_2 ~to_:to_2 in
          if Optional_diff.is_none d
          then diff
          else (
            let d = Optional_diff.unsafe_value d in
            T2 d :: diff)
        in
        let diff =
          let d = get1 ~from:from_1 ~to_:to_1 in
          if Optional_diff.is_none d
          then diff
          else (
            let d = Optional_diff.unsafe_value d in
            T1 d :: diff)
        in
        match diff with
        | [] -> Optional_diff.none
        | _ :: _ -> Optional_diff.return diff)
    ;;

    let apply_exn apply1_exn apply2_exn derived_on diff =
      let derived_on1, derived_on2 = derived_on in
      let t1, diff =
        match diff with
        | T1 d :: tl -> apply1_exn derived_on1 d, tl
        | _ -> derived_on1, diff
      in
      let t2, diff =
        match diff with
        | T2 d :: tl -> apply2_exn derived_on2 d, tl
        | _ -> derived_on2, diff
      in
      match diff with
      | [] -> t1, t2
      | _ :: _ -> failwith "BUG: non-empty diff after apply"
    ;;

    let compare_rank t1 t2 =
      Int.compare (Entry_diff.Variants.to_rank t1) (Entry_diff.Variants.to_rank t2)
    ;;

    let equal_rank t1 t2 =
      Int.equal (Entry_diff.Variants.to_rank t1) (Entry_diff.Variants.to_rank t2)
    ;;

    let singleton entry_diff = [ entry_diff ]

    let of_entry_diffs_exn l =
      let l = List.sort l ~compare:compare_rank in
      match List.find_consecutive_duplicate l ~equal:equal_rank with
      | None -> l
      | Some (dup, _) ->
        failwith ("Duplicate entry in tuple diff: " ^ Entry_diff.Variants.to_name dup)
    ;;

    let t_of_sexp a1_of_sexp a2_of_sexp a1_diff_of_sexp a2_diff_of_sexp sexp =
      of_entry_diffs_exn
        (t_of_sexp a1_of_sexp a2_of_sexp a1_diff_of_sexp a2_diff_of_sexp sexp)
    ;;

    let create ?t1 ?t2 () =
      let diff = [] in
      let diff =
        match t2 with
        | None -> diff
        | Some d -> T2 d :: diff
      in
      let diff =
        match t1 with
        | None -> diff
        | Some d -> T1 d :: diff
      in
      diff
    ;;

    let create_of_variants ~t1 ~t2 =
      let diff = [] in
      let diff =
        let d = t2 Entry_diff.Variants.t2 in
        if Optional_diff.is_none d
        then diff
        else T2 (Optional_diff.unsafe_value d) :: diff
      in
      let diff =
        let d = t1 Entry_diff.Variants.t1 in
        if Optional_diff.is_none d
        then diff
        else T1 (Optional_diff.unsafe_value d) :: diff
      in
      diff
    ;;
  end

  module For_inlined_tuple = struct
    type ('a1, 'a2) t = 'a1 Gel.t * 'a2 Gel.t [@@deriving sexp, bin_io]

    module Diff = struct
      type ('a1, 'a2) derived_on = ('a1, 'a2) t

      type ('a1, 'a2, 'a1_diff, 'a2_diff) t = ('a1, 'a2, 'a1_diff, 'a2_diff) Diff.t
      [@@deriving sexp, bin_io]

      open Diff
      open Entry_diff

      let get get1 get2 ~from ~to_ =
        if Base.phys_equal from to_
        then Optional_diff.none
        else (
          let { Gel.g = from_1 }, { Gel.g = from_2 } = from in
          let { Gel.g = to_1 }, { Gel.g = to_2 } = to_ in
          let diff = [] in
          let diff =
            let d = get2 ~from:from_2 ~to_:to_2 in
            if Optional_diff.is_none d
            then diff
            else (
              let d = Optional_diff.unsafe_value d in
              T2 d :: diff)
          in
          let diff =
            let d = get1 ~from:from_1 ~to_:to_1 in
            if Optional_diff.is_none d
            then diff
            else (
              let d = Optional_diff.unsafe_value d in
              T1 d :: diff)
          in
          match diff with
          | [] -> Optional_diff.none
          | _ :: _ -> Optional_diff.return diff)
      ;;

      let apply_exn apply1_exn apply2_exn derived_on diff =
        let { Gel.g = derived_on1 }, { Gel.g = derived_on2 } = derived_on in
        let t1, diff =
          match diff with
          | T1 d :: tl -> apply1_exn derived_on1 d, tl
          | _ -> derived_on1, diff
        in
        let t2, diff =
          match diff with
          | T2 d :: tl -> apply2_exn derived_on2 d, tl
          | _ -> derived_on2, diff
        in
        match diff with
        | [] -> { Gel.g = t1 }, { Gel.g = t2 }
        | _ :: _ -> failwith "BUG: non-empty diff after apply"
      ;;
    end
  end
end

module Tuple3 = struct
  type ('a1, 'a2, 'a3) t = 'a1 * 'a2 * 'a3 [@@deriving sexp, bin_io]

  module Diff = struct
    type ('a1, 'a2, 'a3) derived_on = ('a1, 'a2, 'a3) t

    module Entry_diff = struct
      type ('a1, 'a2, 'a3, 'a1_diff, 'a2_diff, 'a3_diff) t =
        | T1 of 'a1_diff
        | T2 of 'a2_diff
        | T3 of 'a3_diff
      [@@deriving variants, sexp, bin_io]
    end

    open Entry_diff

    type ('a1, 'a2, 'a3, 'a1_diff, 'a2_diff, 'a3_diff) t =
      ('a1, 'a2, 'a3, 'a1_diff, 'a2_diff, 'a3_diff) Entry_diff.t list
    [@@deriving sexp, bin_io]

    let get get1 get2 get3 ~from ~to_ =
      if Base.phys_equal from to_
      then Optional_diff.none
      else (
        let from_1, from_2, from_3 = from in
        let to_1, to_2, to_3 = to_ in
        let diff = [] in
        let diff =
          let d = get3 ~from:from_3 ~to_:to_3 in
          if Optional_diff.is_none d
          then diff
          else (
            let d = Optional_diff.unsafe_value d in
            T3 d :: diff)
        in
        let diff =
          let d = get2 ~from:from_2 ~to_:to_2 in
          if Optional_diff.is_none d
          then diff
          else (
            let d = Optional_diff.unsafe_value d in
            T2 d :: diff)
        in
        let diff =
          let d = get1 ~from:from_1 ~to_:to_1 in
          if Optional_diff.is_none d
          then diff
          else (
            let d = Optional_diff.unsafe_value d in
            T1 d :: diff)
        in
        match diff with
        | [] -> Optional_diff.none
        | _ :: _ -> Optional_diff.return diff)
    ;;

    let apply_exn apply1_exn apply2_exn apply3_exn derived_on diff =
      let derived_on1, derived_on2, derived_on3 = derived_on in
      let t1, diff =
        match diff with
        | T1 d :: tl -> apply1_exn derived_on1 d, tl
        | _ -> derived_on1, diff
      in
      let t2, diff =
        match diff with
        | T2 d :: tl -> apply2_exn derived_on2 d, tl
        | _ -> derived_on2, diff
      in
      let t3, diff =
        match diff with
        | T3 d :: tl -> apply3_exn derived_on3 d, tl
        | _ -> derived_on3, diff
      in
      match diff with
      | [] -> t1, t2, t3
      | _ :: _ -> failwith "BUG: non-empty diff after apply"
    ;;

    let compare_rank t1 t2 =
      Int.compare (Entry_diff.Variants.to_rank t1) (Entry_diff.Variants.to_rank t2)
    ;;

    let equal_rank t1 t2 =
      Int.equal (Entry_diff.Variants.to_rank t1) (Entry_diff.Variants.to_rank t2)
    ;;

    let singleton entry_diff = [ entry_diff ]

    let of_entry_diffs_exn l =
      let l = List.sort l ~compare:compare_rank in
      match List.find_consecutive_duplicate l ~equal:equal_rank with
      | None -> l
      | Some (dup, _) ->
        failwith ("Duplicate entry in tuple diff: " ^ Entry_diff.Variants.to_name dup)
    ;;

    let t_of_sexp
      a1_of_sexp
      a2_of_sexp
      a3_of_sexp
      a1_diff_of_sexp
      a2_diff_of_sexp
      a3_diff_of_sexp
      sexp
      =
      of_entry_diffs_exn
        (t_of_sexp
           a1_of_sexp
           a2_of_sexp
           a3_of_sexp
           a1_diff_of_sexp
           a2_diff_of_sexp
           a3_diff_of_sexp
           sexp)
    ;;

    let create ?t1 ?t2 ?t3 () =
      let diff = [] in
      let diff =
        match t3 with
        | None -> diff
        | Some d -> T3 d :: diff
      in
      let diff =
        match t2 with
        | None -> diff
        | Some d -> T2 d :: diff
      in
      let diff =
        match t1 with
        | None -> diff
        | Some d -> T1 d :: diff
      in
      diff
    ;;

    let create_of_variants ~t1 ~t2 ~t3 =
      let diff = [] in
      let diff =
        let d = t3 Entry_diff.Variants.t3 in
        if Optional_diff.is_none d
        then diff
        else T3 (Optional_diff.unsafe_value d) :: diff
      in
      let diff =
        let d = t2 Entry_diff.Variants.t2 in
        if Optional_diff.is_none d
        then diff
        else T2 (Optional_diff.unsafe_value d) :: diff
      in
      let diff =
        let d = t1 Entry_diff.Variants.t1 in
        if Optional_diff.is_none d
        then diff
        else T1 (Optional_diff.unsafe_value d) :: diff
      in
      diff
    ;;
  end

  module For_inlined_tuple = struct
    type ('a1, 'a2, 'a3) t = 'a1 Gel.t * 'a2 Gel.t * 'a3 Gel.t [@@deriving sexp, bin_io]

    module Diff = struct
      type ('a1, 'a2, 'a3) derived_on = ('a1, 'a2, 'a3) t

      type ('a1, 'a2, 'a3, 'a1_diff, 'a2_diff, 'a3_diff) t =
        ('a1, 'a2, 'a3, 'a1_diff, 'a2_diff, 'a3_diff) Diff.t
      [@@deriving sexp, bin_io]

      open Diff
      open Entry_diff

      let get get1 get2 get3 ~from ~to_ =
        if Base.phys_equal from to_
        then Optional_diff.none
        else (
          let { Gel.g = from_1 }, { Gel.g = from_2 }, { Gel.g = from_3 } = from in
          let { Gel.g = to_1 }, { Gel.g = to_2 }, { Gel.g = to_3 } = to_ in
          let diff = [] in
          let diff =
            let d = get3 ~from:from_3 ~to_:to_3 in
            if Optional_diff.is_none d
            then diff
            else (
              let d = Optional_diff.unsafe_value d in
              T3 d :: diff)
          in
          let diff =
            let d = get2 ~from:from_2 ~to_:to_2 in
            if Optional_diff.is_none d
            then diff
            else (
              let d = Optional_diff.unsafe_value d in
              T2 d :: diff)
          in
          let diff =
            let d = get1 ~from:from_1 ~to_:to_1 in
            if Optional_diff.is_none d
            then diff
            else (
              let d = Optional_diff.unsafe_value d in
              T1 d :: diff)
          in
          match diff with
          | [] -> Optional_diff.none
          | _ :: _ -> Optional_diff.return diff)
      ;;

      let apply_exn apply1_exn apply2_exn apply3_exn derived_on diff =
        let { Gel.g = derived_on1 }, { Gel.g = derived_on2 }, { Gel.g = derived_on3 } =
          derived_on
        in
        let t1, diff =
          match diff with
          | T1 d :: tl -> apply1_exn derived_on1 d, tl
          | _ -> derived_on1, diff
        in
        let t2, diff =
          match diff with
          | T2 d :: tl -> apply2_exn derived_on2 d, tl
          | _ -> derived_on2, diff
        in
        let t3, diff =
          match diff with
          | T3 d :: tl -> apply3_exn derived_on3 d, tl
          | _ -> derived_on3, diff
        in
        match diff with
        | [] -> { Gel.g = t1 }, { Gel.g = t2 }, { Gel.g = t3 }
        | _ :: _ -> failwith "BUG: non-empty diff after apply"
      ;;
    end
  end
end

module Tuple4 = struct
  type ('a1, 'a2, 'a3, 'a4) t = 'a1 * 'a2 * 'a3 * 'a4 [@@deriving sexp, bin_io]

  module Diff = struct
    type ('a1, 'a2, 'a3, 'a4) derived_on = ('a1, 'a2, 'a3, 'a4) t

    module Entry_diff = struct
      type ('a1, 'a2, 'a3, 'a4, 'a1_diff, 'a2_diff, 'a3_diff, 'a4_diff) t =
        | T1 of 'a1_diff
        | T2 of 'a2_diff
        | T3 of 'a3_diff
        | T4 of 'a4_diff
      [@@deriving variants, sexp, bin_io]
    end

    open Entry_diff

    type ('a1, 'a2, 'a3, 'a4, 'a1_diff, 'a2_diff, 'a3_diff, 'a4_diff) t =
      ('a1, 'a2, 'a3, 'a4, 'a1_diff, 'a2_diff, 'a3_diff, 'a4_diff) Entry_diff.t list
    [@@deriving sexp, bin_io]

    let get get1 get2 get3 get4 ~from ~to_ =
      if Base.phys_equal from to_
      then Optional_diff.none
      else (
        let from_1, from_2, from_3, from_4 = from in
        let to_1, to_2, to_3, to_4 = to_ in
        let diff = [] in
        let diff =
          let d = get4 ~from:from_4 ~to_:to_4 in
          if Optional_diff.is_none d
          then diff
          else (
            let d = Optional_diff.unsafe_value d in
            T4 d :: diff)
        in
        let diff =
          let d = get3 ~from:from_3 ~to_:to_3 in
          if Optional_diff.is_none d
          then diff
          else (
            let d = Optional_diff.unsafe_value d in
            T3 d :: diff)
        in
        let diff =
          let d = get2 ~from:from_2 ~to_:to_2 in
          if Optional_diff.is_none d
          then diff
          else (
            let d = Optional_diff.unsafe_value d in
            T2 d :: diff)
        in
        let diff =
          let d = get1 ~from:from_1 ~to_:to_1 in
          if Optional_diff.is_none d
          then diff
          else (
            let d = Optional_diff.unsafe_value d in
            T1 d :: diff)
        in
        match diff with
        | [] -> Optional_diff.none
        | _ :: _ -> Optional_diff.return diff)
    ;;

    let apply_exn apply1_exn apply2_exn apply3_exn apply4_exn derived_on diff =
      let derived_on1, derived_on2, derived_on3, derived_on4 = derived_on in
      let t1, diff =
        match diff with
        | T1 d :: tl -> apply1_exn derived_on1 d, tl
        | _ -> derived_on1, diff
      in
      let t2, diff =
        match diff with
        | T2 d :: tl -> apply2_exn derived_on2 d, tl
        | _ -> derived_on2, diff
      in
      let t3, diff =
        match diff with
        | T3 d :: tl -> apply3_exn derived_on3 d, tl
        | _ -> derived_on3, diff
      in
      let t4, diff =
        match diff with
        | T4 d :: tl -> apply4_exn derived_on4 d, tl
        | _ -> derived_on4, diff
      in
      match diff with
      | [] -> t1, t2, t3, t4
      | _ :: _ -> failwith "BUG: non-empty diff after apply"
    ;;

    let compare_rank t1 t2 =
      Int.compare (Entry_diff.Variants.to_rank t1) (Entry_diff.Variants.to_rank t2)
    ;;

    let equal_rank t1 t2 =
      Int.equal (Entry_diff.Variants.to_rank t1) (Entry_diff.Variants.to_rank t2)
    ;;

    let singleton entry_diff = [ entry_diff ]

    let of_entry_diffs_exn l =
      let l = List.sort l ~compare:compare_rank in
      match List.find_consecutive_duplicate l ~equal:equal_rank with
      | None -> l
      | Some (dup, _) ->
        failwith ("Duplicate entry in tuple diff: " ^ Entry_diff.Variants.to_name dup)
    ;;

    let t_of_sexp
      a1_of_sexp
      a2_of_sexp
      a3_of_sexp
      a4_of_sexp
      a1_diff_of_sexp
      a2_diff_of_sexp
      a3_diff_of_sexp
      a4_diff_of_sexp
      sexp
      =
      of_entry_diffs_exn
        (t_of_sexp
           a1_of_sexp
           a2_of_sexp
           a3_of_sexp
           a4_of_sexp
           a1_diff_of_sexp
           a2_diff_of_sexp
           a3_diff_of_sexp
           a4_diff_of_sexp
           sexp)
    ;;

    let create ?t1 ?t2 ?t3 ?t4 () =
      let diff = [] in
      let diff =
        match t4 with
        | None -> diff
        | Some d -> T4 d :: diff
      in
      let diff =
        match t3 with
        | None -> diff
        | Some d -> T3 d :: diff
      in
      let diff =
        match t2 with
        | None -> diff
        | Some d -> T2 d :: diff
      in
      let diff =
        match t1 with
        | None -> diff
        | Some d -> T1 d :: diff
      in
      diff
    ;;

    let create_of_variants ~t1 ~t2 ~t3 ~t4 =
      let diff = [] in
      let diff =
        let d = t4 Entry_diff.Variants.t4 in
        if Optional_diff.is_none d
        then diff
        else T4 (Optional_diff.unsafe_value d) :: diff
      in
      let diff =
        let d = t3 Entry_diff.Variants.t3 in
        if Optional_diff.is_none d
        then diff
        else T3 (Optional_diff.unsafe_value d) :: diff
      in
      let diff =
        let d = t2 Entry_diff.Variants.t2 in
        if Optional_diff.is_none d
        then diff
        else T2 (Optional_diff.unsafe_value d) :: diff
      in
      let diff =
        let d = t1 Entry_diff.Variants.t1 in
        if Optional_diff.is_none d
        then diff
        else T1 (Optional_diff.unsafe_value d) :: diff
      in
      diff
    ;;
  end

  module For_inlined_tuple = struct
    type ('a1, 'a2, 'a3, 'a4) t = 'a1 Gel.t * 'a2 Gel.t * 'a3 Gel.t * 'a4 Gel.t
    [@@deriving sexp, bin_io]

    module Diff = struct
      type ('a1, 'a2, 'a3, 'a4) derived_on = ('a1, 'a2, 'a3, 'a4) t

      type ('a1, 'a2, 'a3, 'a4, 'a1_diff, 'a2_diff, 'a3_diff, 'a4_diff) t =
        ('a1, 'a2, 'a3, 'a4, 'a1_diff, 'a2_diff, 'a3_diff, 'a4_diff) Diff.t
      [@@deriving sexp, bin_io]

      open Diff
      open Entry_diff

      let get get1 get2 get3 get4 ~from ~to_ =
        if Base.phys_equal from to_
        then Optional_diff.none
        else (
          let ( { Gel.g = from_1 }
              , { Gel.g = from_2 }
              , { Gel.g = from_3 }
              , { Gel.g = from_4 } )
            =
            from
          in
          let { Gel.g = to_1 }, { Gel.g = to_2 }, { Gel.g = to_3 }, { Gel.g = to_4 } =
            to_
          in
          let diff = [] in
          let diff =
            let d = get4 ~from:from_4 ~to_:to_4 in
            if Optional_diff.is_none d
            then diff
            else (
              let d = Optional_diff.unsafe_value d in
              T4 d :: diff)
          in
          let diff =
            let d = get3 ~from:from_3 ~to_:to_3 in
            if Optional_diff.is_none d
            then diff
            else (
              let d = Optional_diff.unsafe_value d in
              T3 d :: diff)
          in
          let diff =
            let d = get2 ~from:from_2 ~to_:to_2 in
            if Optional_diff.is_none d
            then diff
            else (
              let d = Optional_diff.unsafe_value d in
              T2 d :: diff)
          in
          let diff =
            let d = get1 ~from:from_1 ~to_:to_1 in
            if Optional_diff.is_none d
            then diff
            else (
              let d = Optional_diff.unsafe_value d in
              T1 d :: diff)
          in
          match diff with
          | [] -> Optional_diff.none
          | _ :: _ -> Optional_diff.return diff)
      ;;

      let apply_exn apply1_exn apply2_exn apply3_exn apply4_exn derived_on diff =
        let ( { Gel.g = derived_on1 }
            , { Gel.g = derived_on2 }
            , { Gel.g = derived_on3 }
            , { Gel.g = derived_on4 } )
          =
          derived_on
        in
        let t1, diff =
          match diff with
          | T1 d :: tl -> apply1_exn derived_on1 d, tl
          | _ -> derived_on1, diff
        in
        let t2, diff =
          match diff with
          | T2 d :: tl -> apply2_exn derived_on2 d, tl
          | _ -> derived_on2, diff
        in
        let t3, diff =
          match diff with
          | T3 d :: tl -> apply3_exn derived_on3 d, tl
          | _ -> derived_on3, diff
        in
        let t4, diff =
          match diff with
          | T4 d :: tl -> apply4_exn derived_on4 d, tl
          | _ -> derived_on4, diff
        in
        match diff with
        | [] -> { Gel.g = t1 }, { Gel.g = t2 }, { Gel.g = t3 }, { Gel.g = t4 }
        | _ :: _ -> failwith "BUG: non-empty diff after apply"
      ;;
    end
  end
end

module Tuple5 = struct
  type ('a1, 'a2, 'a3, 'a4, 'a5) t = 'a1 * 'a2 * 'a3 * 'a4 * 'a5 [@@deriving sexp, bin_io]

  module Diff = struct
    type ('a1, 'a2, 'a3, 'a4, 'a5) derived_on = ('a1, 'a2, 'a3, 'a4, 'a5) t

    module Entry_diff = struct
      type ('a1, 'a2, 'a3, 'a4, 'a5, 'a1_diff, 'a2_diff, 'a3_diff, 'a4_diff, 'a5_diff) t =
        | T1 of 'a1_diff
        | T2 of 'a2_diff
        | T3 of 'a3_diff
        | T4 of 'a4_diff
        | T5 of 'a5_diff
      [@@deriving variants, sexp, bin_io]
    end

    open Entry_diff

    type ('a1, 'a2, 'a3, 'a4, 'a5, 'a1_diff, 'a2_diff, 'a3_diff, 'a4_diff, 'a5_diff) t =
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
    [@@deriving sexp, bin_io]

    let get get1 get2 get3 get4 get5 ~from ~to_ =
      if Base.phys_equal from to_
      then Optional_diff.none
      else (
        let from_1, from_2, from_3, from_4, from_5 = from in
        let to_1, to_2, to_3, to_4, to_5 = to_ in
        let diff = [] in
        let diff =
          let d = get5 ~from:from_5 ~to_:to_5 in
          if Optional_diff.is_none d
          then diff
          else (
            let d = Optional_diff.unsafe_value d in
            T5 d :: diff)
        in
        let diff =
          let d = get4 ~from:from_4 ~to_:to_4 in
          if Optional_diff.is_none d
          then diff
          else (
            let d = Optional_diff.unsafe_value d in
            T4 d :: diff)
        in
        let diff =
          let d = get3 ~from:from_3 ~to_:to_3 in
          if Optional_diff.is_none d
          then diff
          else (
            let d = Optional_diff.unsafe_value d in
            T3 d :: diff)
        in
        let diff =
          let d = get2 ~from:from_2 ~to_:to_2 in
          if Optional_diff.is_none d
          then diff
          else (
            let d = Optional_diff.unsafe_value d in
            T2 d :: diff)
        in
        let diff =
          let d = get1 ~from:from_1 ~to_:to_1 in
          if Optional_diff.is_none d
          then diff
          else (
            let d = Optional_diff.unsafe_value d in
            T1 d :: diff)
        in
        match diff with
        | [] -> Optional_diff.none
        | _ :: _ -> Optional_diff.return diff)
    ;;

    let apply_exn apply1_exn apply2_exn apply3_exn apply4_exn apply5_exn derived_on diff =
      let derived_on1, derived_on2, derived_on3, derived_on4, derived_on5 = derived_on in
      let t1, diff =
        match diff with
        | T1 d :: tl -> apply1_exn derived_on1 d, tl
        | _ -> derived_on1, diff
      in
      let t2, diff =
        match diff with
        | T2 d :: tl -> apply2_exn derived_on2 d, tl
        | _ -> derived_on2, diff
      in
      let t3, diff =
        match diff with
        | T3 d :: tl -> apply3_exn derived_on3 d, tl
        | _ -> derived_on3, diff
      in
      let t4, diff =
        match diff with
        | T4 d :: tl -> apply4_exn derived_on4 d, tl
        | _ -> derived_on4, diff
      in
      let t5, diff =
        match diff with
        | T5 d :: tl -> apply5_exn derived_on5 d, tl
        | _ -> derived_on5, diff
      in
      match diff with
      | [] -> t1, t2, t3, t4, t5
      | _ :: _ -> failwith "BUG: non-empty diff after apply"
    ;;

    let compare_rank t1 t2 =
      Int.compare (Entry_diff.Variants.to_rank t1) (Entry_diff.Variants.to_rank t2)
    ;;

    let equal_rank t1 t2 =
      Int.equal (Entry_diff.Variants.to_rank t1) (Entry_diff.Variants.to_rank t2)
    ;;

    let singleton entry_diff = [ entry_diff ]

    let of_entry_diffs_exn l =
      let l = List.sort l ~compare:compare_rank in
      match List.find_consecutive_duplicate l ~equal:equal_rank with
      | None -> l
      | Some (dup, _) ->
        failwith ("Duplicate entry in tuple diff: " ^ Entry_diff.Variants.to_name dup)
    ;;

    let t_of_sexp
      a1_of_sexp
      a2_of_sexp
      a3_of_sexp
      a4_of_sexp
      a5_of_sexp
      a1_diff_of_sexp
      a2_diff_of_sexp
      a3_diff_of_sexp
      a4_diff_of_sexp
      a5_diff_of_sexp
      sexp
      =
      of_entry_diffs_exn
        (t_of_sexp
           a1_of_sexp
           a2_of_sexp
           a3_of_sexp
           a4_of_sexp
           a5_of_sexp
           a1_diff_of_sexp
           a2_diff_of_sexp
           a3_diff_of_sexp
           a4_diff_of_sexp
           a5_diff_of_sexp
           sexp)
    ;;

    let create ?t1 ?t2 ?t3 ?t4 ?t5 () =
      let diff = [] in
      let diff =
        match t5 with
        | None -> diff
        | Some d -> T5 d :: diff
      in
      let diff =
        match t4 with
        | None -> diff
        | Some d -> T4 d :: diff
      in
      let diff =
        match t3 with
        | None -> diff
        | Some d -> T3 d :: diff
      in
      let diff =
        match t2 with
        | None -> diff
        | Some d -> T2 d :: diff
      in
      let diff =
        match t1 with
        | None -> diff
        | Some d -> T1 d :: diff
      in
      diff
    ;;

    let create_of_variants ~t1 ~t2 ~t3 ~t4 ~t5 =
      let diff = [] in
      let diff =
        let d = t5 Entry_diff.Variants.t5 in
        if Optional_diff.is_none d
        then diff
        else T5 (Optional_diff.unsafe_value d) :: diff
      in
      let diff =
        let d = t4 Entry_diff.Variants.t4 in
        if Optional_diff.is_none d
        then diff
        else T4 (Optional_diff.unsafe_value d) :: diff
      in
      let diff =
        let d = t3 Entry_diff.Variants.t3 in
        if Optional_diff.is_none d
        then diff
        else T3 (Optional_diff.unsafe_value d) :: diff
      in
      let diff =
        let d = t2 Entry_diff.Variants.t2 in
        if Optional_diff.is_none d
        then diff
        else T2 (Optional_diff.unsafe_value d) :: diff
      in
      let diff =
        let d = t1 Entry_diff.Variants.t1 in
        if Optional_diff.is_none d
        then diff
        else T1 (Optional_diff.unsafe_value d) :: diff
      in
      diff
    ;;
  end

  module For_inlined_tuple = struct
    type ('a1, 'a2, 'a3, 'a4, 'a5) t =
      'a1 Gel.t * 'a2 Gel.t * 'a3 Gel.t * 'a4 Gel.t * 'a5 Gel.t
    [@@deriving sexp, bin_io]

    module Diff = struct
      type ('a1, 'a2, 'a3, 'a4, 'a5) derived_on = ('a1, 'a2, 'a3, 'a4, 'a5) t

      type ('a1, 'a2, 'a3, 'a4, 'a5, 'a1_diff, 'a2_diff, 'a3_diff, 'a4_diff, 'a5_diff) t =
        ('a1, 'a2, 'a3, 'a4, 'a5, 'a1_diff, 'a2_diff, 'a3_diff, 'a4_diff, 'a5_diff) Diff.t
      [@@deriving sexp, bin_io]

      open Diff
      open Entry_diff

      let get get1 get2 get3 get4 get5 ~from ~to_ =
        if Base.phys_equal from to_
        then Optional_diff.none
        else (
          let ( { Gel.g = from_1 }
              , { Gel.g = from_2 }
              , { Gel.g = from_3 }
              , { Gel.g = from_4 }
              , { Gel.g = from_5 } )
            =
            from
          in
          let ( { Gel.g = to_1 }
              , { Gel.g = to_2 }
              , { Gel.g = to_3 }
              , { Gel.g = to_4 }
              , { Gel.g = to_5 } )
            =
            to_
          in
          let diff = [] in
          let diff =
            let d = get5 ~from:from_5 ~to_:to_5 in
            if Optional_diff.is_none d
            then diff
            else (
              let d = Optional_diff.unsafe_value d in
              T5 d :: diff)
          in
          let diff =
            let d = get4 ~from:from_4 ~to_:to_4 in
            if Optional_diff.is_none d
            then diff
            else (
              let d = Optional_diff.unsafe_value d in
              T4 d :: diff)
          in
          let diff =
            let d = get3 ~from:from_3 ~to_:to_3 in
            if Optional_diff.is_none d
            then diff
            else (
              let d = Optional_diff.unsafe_value d in
              T3 d :: diff)
          in
          let diff =
            let d = get2 ~from:from_2 ~to_:to_2 in
            if Optional_diff.is_none d
            then diff
            else (
              let d = Optional_diff.unsafe_value d in
              T2 d :: diff)
          in
          let diff =
            let d = get1 ~from:from_1 ~to_:to_1 in
            if Optional_diff.is_none d
            then diff
            else (
              let d = Optional_diff.unsafe_value d in
              T1 d :: diff)
          in
          match diff with
          | [] -> Optional_diff.none
          | _ :: _ -> Optional_diff.return diff)
      ;;

      let apply_exn apply1_exn apply2_exn apply3_exn apply4_exn apply5_exn derived_on diff
        =
        let ( { Gel.g = derived_on1 }
            , { Gel.g = derived_on2 }
            , { Gel.g = derived_on3 }
            , { Gel.g = derived_on4 }
            , { Gel.g = derived_on5 } )
          =
          derived_on
        in
        let t1, diff =
          match diff with
          | T1 d :: tl -> apply1_exn derived_on1 d, tl
          | _ -> derived_on1, diff
        in
        let t2, diff =
          match diff with
          | T2 d :: tl -> apply2_exn derived_on2 d, tl
          | _ -> derived_on2, diff
        in
        let t3, diff =
          match diff with
          | T3 d :: tl -> apply3_exn derived_on3 d, tl
          | _ -> derived_on3, diff
        in
        let t4, diff =
          match diff with
          | T4 d :: tl -> apply4_exn derived_on4 d, tl
          | _ -> derived_on4, diff
        in
        let t5, diff =
          match diff with
          | T5 d :: tl -> apply5_exn derived_on5 d, tl
          | _ -> derived_on5, diff
        in
        match diff with
        | [] ->
          { Gel.g = t1 }, { Gel.g = t2 }, { Gel.g = t3 }, { Gel.g = t4 }, { Gel.g = t5 }
        | _ :: _ -> failwith "BUG: non-empty diff after apply"
      ;;
    end
  end
end

module Tuple6 = struct
  type ('a1, 'a2, 'a3, 'a4, 'a5, 'a6) t = 'a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6
  [@@deriving sexp, bin_io]

  module Diff = struct
    type ('a1, 'a2, 'a3, 'a4, 'a5, 'a6) derived_on = ('a1, 'a2, 'a3, 'a4, 'a5, 'a6) t

    module Entry_diff = struct
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
      [@@deriving variants, sexp, bin_io]
    end

    open Entry_diff

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
      Entry_diff.t
      list
    [@@deriving sexp, bin_io]

    let get get1 get2 get3 get4 get5 get6 ~from ~to_ =
      if Base.phys_equal from to_
      then Optional_diff.none
      else (
        let from_1, from_2, from_3, from_4, from_5, from_6 = from in
        let to_1, to_2, to_3, to_4, to_5, to_6 = to_ in
        let diff = [] in
        let diff =
          let d = get6 ~from:from_6 ~to_:to_6 in
          if Optional_diff.is_none d
          then diff
          else (
            let d = Optional_diff.unsafe_value d in
            T6 d :: diff)
        in
        let diff =
          let d = get5 ~from:from_5 ~to_:to_5 in
          if Optional_diff.is_none d
          then diff
          else (
            let d = Optional_diff.unsafe_value d in
            T5 d :: diff)
        in
        let diff =
          let d = get4 ~from:from_4 ~to_:to_4 in
          if Optional_diff.is_none d
          then diff
          else (
            let d = Optional_diff.unsafe_value d in
            T4 d :: diff)
        in
        let diff =
          let d = get3 ~from:from_3 ~to_:to_3 in
          if Optional_diff.is_none d
          then diff
          else (
            let d = Optional_diff.unsafe_value d in
            T3 d :: diff)
        in
        let diff =
          let d = get2 ~from:from_2 ~to_:to_2 in
          if Optional_diff.is_none d
          then diff
          else (
            let d = Optional_diff.unsafe_value d in
            T2 d :: diff)
        in
        let diff =
          let d = get1 ~from:from_1 ~to_:to_1 in
          if Optional_diff.is_none d
          then diff
          else (
            let d = Optional_diff.unsafe_value d in
            T1 d :: diff)
        in
        match diff with
        | [] -> Optional_diff.none
        | _ :: _ -> Optional_diff.return diff)
    ;;

    let apply_exn
      apply1_exn
      apply2_exn
      apply3_exn
      apply4_exn
      apply5_exn
      apply6_exn
      derived_on
      diff
      =
      let derived_on1, derived_on2, derived_on3, derived_on4, derived_on5, derived_on6 =
        derived_on
      in
      let t1, diff =
        match diff with
        | T1 d :: tl -> apply1_exn derived_on1 d, tl
        | _ -> derived_on1, diff
      in
      let t2, diff =
        match diff with
        | T2 d :: tl -> apply2_exn derived_on2 d, tl
        | _ -> derived_on2, diff
      in
      let t3, diff =
        match diff with
        | T3 d :: tl -> apply3_exn derived_on3 d, tl
        | _ -> derived_on3, diff
      in
      let t4, diff =
        match diff with
        | T4 d :: tl -> apply4_exn derived_on4 d, tl
        | _ -> derived_on4, diff
      in
      let t5, diff =
        match diff with
        | T5 d :: tl -> apply5_exn derived_on5 d, tl
        | _ -> derived_on5, diff
      in
      let t6, diff =
        match diff with
        | T6 d :: tl -> apply6_exn derived_on6 d, tl
        | _ -> derived_on6, diff
      in
      match diff with
      | [] -> t1, t2, t3, t4, t5, t6
      | _ :: _ -> failwith "BUG: non-empty diff after apply"
    ;;

    let compare_rank t1 t2 =
      Int.compare (Entry_diff.Variants.to_rank t1) (Entry_diff.Variants.to_rank t2)
    ;;

    let equal_rank t1 t2 =
      Int.equal (Entry_diff.Variants.to_rank t1) (Entry_diff.Variants.to_rank t2)
    ;;

    let singleton entry_diff = [ entry_diff ]

    let of_entry_diffs_exn l =
      let l = List.sort l ~compare:compare_rank in
      match List.find_consecutive_duplicate l ~equal:equal_rank with
      | None -> l
      | Some (dup, _) ->
        failwith ("Duplicate entry in tuple diff: " ^ Entry_diff.Variants.to_name dup)
    ;;

    let t_of_sexp
      a1_of_sexp
      a2_of_sexp
      a3_of_sexp
      a4_of_sexp
      a5_of_sexp
      a6_of_sexp
      a1_diff_of_sexp
      a2_diff_of_sexp
      a3_diff_of_sexp
      a4_diff_of_sexp
      a5_diff_of_sexp
      a6_diff_of_sexp
      sexp
      =
      of_entry_diffs_exn
        (t_of_sexp
           a1_of_sexp
           a2_of_sexp
           a3_of_sexp
           a4_of_sexp
           a5_of_sexp
           a6_of_sexp
           a1_diff_of_sexp
           a2_diff_of_sexp
           a3_diff_of_sexp
           a4_diff_of_sexp
           a5_diff_of_sexp
           a6_diff_of_sexp
           sexp)
    ;;

    let create ?t1 ?t2 ?t3 ?t4 ?t5 ?t6 () =
      let diff = [] in
      let diff =
        match t6 with
        | None -> diff
        | Some d -> T6 d :: diff
      in
      let diff =
        match t5 with
        | None -> diff
        | Some d -> T5 d :: diff
      in
      let diff =
        match t4 with
        | None -> diff
        | Some d -> T4 d :: diff
      in
      let diff =
        match t3 with
        | None -> diff
        | Some d -> T3 d :: diff
      in
      let diff =
        match t2 with
        | None -> diff
        | Some d -> T2 d :: diff
      in
      let diff =
        match t1 with
        | None -> diff
        | Some d -> T1 d :: diff
      in
      diff
    ;;

    let create_of_variants ~t1 ~t2 ~t3 ~t4 ~t5 ~t6 =
      let diff = [] in
      let diff =
        let d = t6 Entry_diff.Variants.t6 in
        if Optional_diff.is_none d
        then diff
        else T6 (Optional_diff.unsafe_value d) :: diff
      in
      let diff =
        let d = t5 Entry_diff.Variants.t5 in
        if Optional_diff.is_none d
        then diff
        else T5 (Optional_diff.unsafe_value d) :: diff
      in
      let diff =
        let d = t4 Entry_diff.Variants.t4 in
        if Optional_diff.is_none d
        then diff
        else T4 (Optional_diff.unsafe_value d) :: diff
      in
      let diff =
        let d = t3 Entry_diff.Variants.t3 in
        if Optional_diff.is_none d
        then diff
        else T3 (Optional_diff.unsafe_value d) :: diff
      in
      let diff =
        let d = t2 Entry_diff.Variants.t2 in
        if Optional_diff.is_none d
        then diff
        else T2 (Optional_diff.unsafe_value d) :: diff
      in
      let diff =
        let d = t1 Entry_diff.Variants.t1 in
        if Optional_diff.is_none d
        then diff
        else T1 (Optional_diff.unsafe_value d) :: diff
      in
      diff
    ;;
  end

  module For_inlined_tuple = struct
    type ('a1, 'a2, 'a3, 'a4, 'a5, 'a6) t =
      'a1 Gel.t * 'a2 Gel.t * 'a3 Gel.t * 'a4 Gel.t * 'a5 Gel.t * 'a6 Gel.t
    [@@deriving sexp, bin_io]

    module Diff = struct
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
      [@@deriving sexp, bin_io]

      open Diff
      open Entry_diff

      let get get1 get2 get3 get4 get5 get6 ~from ~to_ =
        if Base.phys_equal from to_
        then Optional_diff.none
        else (
          let ( { Gel.g = from_1 }
              , { Gel.g = from_2 }
              , { Gel.g = from_3 }
              , { Gel.g = from_4 }
              , { Gel.g = from_5 }
              , { Gel.g = from_6 } )
            =
            from
          in
          let ( { Gel.g = to_1 }
              , { Gel.g = to_2 }
              , { Gel.g = to_3 }
              , { Gel.g = to_4 }
              , { Gel.g = to_5 }
              , { Gel.g = to_6 } )
            =
            to_
          in
          let diff = [] in
          let diff =
            let d = get6 ~from:from_6 ~to_:to_6 in
            if Optional_diff.is_none d
            then diff
            else (
              let d = Optional_diff.unsafe_value d in
              T6 d :: diff)
          in
          let diff =
            let d = get5 ~from:from_5 ~to_:to_5 in
            if Optional_diff.is_none d
            then diff
            else (
              let d = Optional_diff.unsafe_value d in
              T5 d :: diff)
          in
          let diff =
            let d = get4 ~from:from_4 ~to_:to_4 in
            if Optional_diff.is_none d
            then diff
            else (
              let d = Optional_diff.unsafe_value d in
              T4 d :: diff)
          in
          let diff =
            let d = get3 ~from:from_3 ~to_:to_3 in
            if Optional_diff.is_none d
            then diff
            else (
              let d = Optional_diff.unsafe_value d in
              T3 d :: diff)
          in
          let diff =
            let d = get2 ~from:from_2 ~to_:to_2 in
            if Optional_diff.is_none d
            then diff
            else (
              let d = Optional_diff.unsafe_value d in
              T2 d :: diff)
          in
          let diff =
            let d = get1 ~from:from_1 ~to_:to_1 in
            if Optional_diff.is_none d
            then diff
            else (
              let d = Optional_diff.unsafe_value d in
              T1 d :: diff)
          in
          match diff with
          | [] -> Optional_diff.none
          | _ :: _ -> Optional_diff.return diff)
      ;;

      let apply_exn
        apply1_exn
        apply2_exn
        apply3_exn
        apply4_exn
        apply5_exn
        apply6_exn
        derived_on
        diff
        =
        let ( { Gel.g = derived_on1 }
            , { Gel.g = derived_on2 }
            , { Gel.g = derived_on3 }
            , { Gel.g = derived_on4 }
            , { Gel.g = derived_on5 }
            , { Gel.g = derived_on6 } )
          =
          derived_on
        in
        let t1, diff =
          match diff with
          | T1 d :: tl -> apply1_exn derived_on1 d, tl
          | _ -> derived_on1, diff
        in
        let t2, diff =
          match diff with
          | T2 d :: tl -> apply2_exn derived_on2 d, tl
          | _ -> derived_on2, diff
        in
        let t3, diff =
          match diff with
          | T3 d :: tl -> apply3_exn derived_on3 d, tl
          | _ -> derived_on3, diff
        in
        let t4, diff =
          match diff with
          | T4 d :: tl -> apply4_exn derived_on4 d, tl
          | _ -> derived_on4, diff
        in
        let t5, diff =
          match diff with
          | T5 d :: tl -> apply5_exn derived_on5 d, tl
          | _ -> derived_on5, diff
        in
        let t6, diff =
          match diff with
          | T6 d :: tl -> apply6_exn derived_on6 d, tl
          | _ -> derived_on6, diff
        in
        match diff with
        | [] ->
          ( { Gel.g = t1 }
          , { Gel.g = t2 }
          , { Gel.g = t3 }
          , { Gel.g = t4 }
          , { Gel.g = t5 }
          , { Gel.g = t6 } )
        | _ :: _ -> failwith "BUG: non-empty diff after apply"
      ;;
    end
  end
end
(*$*)

let max_supported = Ldiffable_cinaps.Tuple_helpers.max_supported
