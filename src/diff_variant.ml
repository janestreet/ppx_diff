(* For a variant such as

   {[ type derived_on =
        | A
        | B of b
        | C of c
        | D

   ]}

   the diff type will be

   {[ type t =
        | Set_to_a
        | Set_to_b of b
        | Set_to_c of c
        | Set_to_d
        | Diff_b of b_diff
        | Diff_c of c_diff
   }]
*)
open Base
open Ppxlib
open Build_helper

module Maybe_polymorphic = struct
  type ('row_type, 'diff_type, 'prefix) t =
    | Polymorphic : (How_to_diff.t Type_kind.core, unit Type_kind.core_kind, unit) t
    | Not_polymorphic
        : (How_to_diff.t Type_kind.variant_row_type, unit Type_kind.t, Items.t) t

  let is_polymorphic
    (type row_type diff_type prefix)
    (t : (row_type, diff_type, prefix) t)
    =
    match t with
    | Polymorphic -> true
    | Not_polymorphic -> false
  ;;
end

(* Avoiding name collissions for inlined record helper modules.

   E.g. for

   {[ type t =
        | A of { foo : int }
        | B
   ]}

   we'll generate a helper module

   {[ module A_record = struct
        type t = { foo : int }

        module Diff = struct
          (* some diff functions here *)
        end
      end
   ]}

   But for

   {[ type t =
        | A of { foo : int }
        | B of A_record.t
   ]}

   that would cause a name collisions.

   This module adds '_' at the end of the helper module name until there are no collisions
*)
module Module_name_generator : sig
  type t

  val create
    :  ('row_type, _, _) Maybe_polymorphic.t
    -> (Variant_row_name.t * 'row_type option) list
    -> t

  val record_module_name : t -> Variant_row_name.t -> Module_name.t
end = struct
  type t =
    { collisions : (Module_name.t, Module_name.comparator_witness) Set.t
    ; record_module_names : (Variant_row_name.t, Module_name.t) Hashtbl.t
    }

  let create
    (type row_type diff_type variant)
    (maybe_polymorphic : (row_type, diff_type, variant) Maybe_polymorphic.t)
    (rows : (Variant_row_name.t * row_type option) list)
    =
    let variant =
      match maybe_polymorphic with
      | Polymorphic -> Type_kind.(Core (Polymorphic_variant rows, None))
      | Not_polymorphic -> Type_kind.Variant { rows; equal_to = None }
    in
    let collisions =
      let rec hds = function
        | Longident_helper.Simple (hd, _) -> [ hd ]
        | Functor_application (functor_, arg, _tl) -> hds functor_ @ hds arg
      in
      variant
      |> Type_kind.constrs
      |> List.concat_map ~f:(fun { Type_kind.module_; _ } ->
           match module_ with
           | None -> []
           | Some module_ -> hds module_)
      |> Set.of_list (module Module_name)
    in
    { collisions; record_module_names = Hashtbl.create (module Variant_row_name) }
  ;;

  let generate_record_module_name t row_name =
    let rec loop candidate =
      if not (Set.mem t.collisions candidate)
      then candidate
      else loop (Module_name.of_string (Module_name.to_string candidate ^ "_"))
    in
    loop (Module_name.of_string (Variant_row_name.to_string row_name ^ "_record"))
  ;;

  let record_module_name t row_name =
    Hashtbl.find_or_add t.record_module_names row_name ~default:(fun () ->
      generate_record_module_name t row_name)
  ;;
end

(* [Row] represents a row in the [derived_on] type.

   From this we can get:
   1. the original row in the derived_on type ([Which.Derived_on])
   2. the [Set_*] row in the diff type ([Which.Set])
   3. the [Diff_*] row in the diff type ([Which.Diff])
*)
module Row : sig
  module Row_diff : sig
    type 'row_type t

    (* The derived on type that we need to diff. We handle all of:
       1. simple rows, e.g. [B of b]
       2. inlined tuples, e.g. [T of t1 * t2 * ...]
       3. inlined records, e.g. [R of { field_a : t_a ; field_b : t_b }]
    *)
    val row_type : 'row_type t -> 'row_type
    val core_diff : _ t -> Core_diff.t

    (* Helper representing the type actually used by [core_diff]. Will look like:

       1. [txt]
       2. local_ (t1, t2, ...) Tuples.TupleN.For_inlined_tuple.t = local_ ( { Gel.g = txt1 }, { Gel.g = txt2 }, ...)
       3. local_ { R_record.field_a = txt_a ; field_b = txt_b }

       (For [2] and [3] the local_ keyword is only used for expresions, not for patterns.)
    *)
    val derived_on
      :  ?skip_local:bool
      -> 'row_type t
      -> ('row_type, _, _) Maybe_polymorphic.t
      -> Prefix.t
      -> Build_helper.t
  end

  module Which : sig
    type t =
      | Derived_on
      | Set
      | Diff
  end

  type 'row_type t

  val create
    :  Variant_row_name.t
    -> 'row_type option
    -> ('row_type, _, 'prefix) Maybe_polymorphic.t
    -> context:Context.t
    -> create_core:Core_diff.Create.t
    -> module_name_generator:Module_name_generator.t
    -> 'row_type t * 'prefix option

  val diff : 'row_type t -> 'row_type Row_diff.t option
  val name : _ t -> Which.t -> Variant_row_name.t

  val get_row
    :  'row_type t
    -> ('row_type, _, _) Maybe_polymorphic.t
    -> Which.t
    -> Prefix.t
    -> Build_helper.t

  val function_name : _ t -> Function_name.t -> Build_helper.t
end = struct
  module Row_diff = struct
    type 'row_type t =
      { row_name : Variant_row_name.t
      ; row_type : 'row_type
      ; core_diff : Core_diff.t
      ; module_name_generator : Module_name_generator.t
      }

    let core_diff t = t.core_diff
    let row_type t = t.row_type

    let create
      (type row_type diff_type prefix)
      name
      (row_type : row_type)
      (maybe_polymorphic : (row_type, diff_type, prefix) Maybe_polymorphic.t)
      ~(create_core : Core_diff.Create.t)
      ~module_name_generator
      ~context
      : row_type t * prefix
      =
      let core_diff, (prefix : prefix) =
        match maybe_polymorphic with
        | Polymorphic -> create_core row_type, ()
        | Not_polymorphic ->
          let { Context.builder; all_params; _ } = context in
          (match row_type with
           | Single core -> create_core core, Items.empty
           | Inlined_tuple tuple ->
             Diff_tuple.create tuple ~builder ~create_core ~inlined:true, Items.empty
           | Inlined_record record_fields ->
             (* Create an additional module, which is effectively

                {[ module R_record = struct
                     type t = { global_ field_a : t_a ; global_ field_b : t_b ; ... } [@deriving diff]
                   end
                ]}

                and use it as prefix
             *)
             let record_fields =
               List.map record_fields ~f:(fun field -> { field with global = true })
             in
             let local = true in
             let diff =
               Diff_record.create_record ~record_fields ~local ~builder ~create_core
             in
             let type_name = Type_name.t in
             let kind_to_diff =
               Type_kind.Record { fields = record_fields; local; equal_to = None }
               |> Type_kind.map ~f:(fun _ -> ())
             in
             let type_to_diff_vars =
               kind_to_diff |> Type_kind.vars |> Set.of_list (module Var)
             in
             let type_to_diff_params =
               List.filter all_params ~f:(fun param ->
                 Set.mem type_to_diff_vars (Type_param.var param))
             in
             let type_to_diff_declaration =
               { Type_declaration.kind = kind_to_diff
               ; params = type_to_diff_params
               ; name = type_name
               ; unboxed = Type_kind.can_be_unboxed kind_to_diff
               }
             in
             let record_module_name =
               Module_name_generator.record_module_name module_name_generator name
             in
             let prefix =
               let items = Diff.to_module diff ~type_to_diff_declaration ~context in
               let td_items =
                 Type_declaration.to_items type_to_diff_declaration ~context
               in
               Items.concat [ td_items; items ]
               |> Items.to_module ~module_name:record_module_name ~builder
             in
             (* And then the [Core_diff] is:

                type t = R_record.Diff.t
                let get = R_record.Diff.get
                let apply = R_record.Diff.apply
             *)
             let t =
               Diff_constr.create
                 { params =
                     List.map type_to_diff_params ~f:(fun param ->
                       Type_kind.Var (Type_param.var param), None)
                 ; module_ = Longident_helper.of_simple_list [ record_module_name ]
                 ; type_name
                 }
                 ~create_core
                 ~builder
             in
             t, prefix)
      in
      { core_diff; row_name = name; row_type; module_name_generator }, prefix
    ;;

    let derived_on_structure
      ?(skip_local = false)
      (type row_type diff_type prefix)
      (t : row_type t)
      (maybe_polymorphic : (row_type, diff_type, prefix) Maybe_polymorphic.t)
      txt
      ~for_diff
      =
      if String.( = ) (Prefix.to_string txt) "_"
      then Text (Prefix.to_string txt)
      else (
        match (maybe_polymorphic : _ Maybe_polymorphic.t) with
        | Polymorphic -> Text (Prefix.to_string txt)
        | Not_polymorphic ->
          (match t.row_type with
           | Single _ -> Text (Prefix.to_string txt)
           | Inlined_record fields ->
             let record =
               Record
                 { module_ =
                     Option.some_if
                       for_diff
                       (Module_name_generator.record_module_name
                          t.module_name_generator
                          t.row_name)
                 ; fields =
                     List.map fields ~f:(fun { field_name; _ } ->
                       { field_name
                       ; field_value =
                           Text
                             (Prefix.to_prefix (Some txt)
                              ^ Record_field_name.to_string field_name)
                       })
                 }
             in
             if for_diff && not skip_local then Local_expr record else record
           | Inlined_tuple tuple ->
             let tuple_entry i =
               let text = Text (Prefix.to_string txt ^ Int.to_string (i + 1)) in
               if not for_diff
               then text
               else
                 Record
                   { module_ = Some (Module_name.of_string "Gel")
                   ; fields =
                       [ { field_name = Record_field_name.of_string "g"
                         ; field_value = text
                         }
                       ]
                   }
             in
             let tuple = Tuple (List.mapi tuple ~f:(fun i _ -> tuple_entry i)) in
             if for_diff && not skip_local then Local_expr tuple else tuple))
    ;;

    let derived_on = derived_on_structure ~for_diff:true
  end

  module Which = struct
    type t =
      | Derived_on
      | Set
      | Diff
  end

  type 'row_type t =
    { name : Variant_row_name.t
    ; diff : 'row_type Row_diff.t option
    ; polymorphic : bool
    }

  let name t which =
    let prefix =
      match (which : Which.t) with
      | Derived_on -> ""
      | Set -> "Set_to_"
      | Diff -> "Diff_"
    in
    let name_string = Variant_row_name.to_string t.name in
    let name =
      prefix ^ if t.polymorphic then name_string else String.uncapitalize name_string
    in
    Variant_row_name.of_string (if t.polymorphic then name else String.capitalize name)
  ;;

  let diff t = t.diff

  let create name row_type maybe_polymorphic ~context ~create_core ~module_name_generator =
    let diff, prefix =
      match row_type with
      | None -> None, None
      | Some row_type ->
        let row_diff, prefix =
          Row_diff.create
            name
            row_type
            maybe_polymorphic
            ~context
            ~create_core
            ~module_name_generator
        in
        Some row_diff, Some prefix
    in
    ( { name; diff; polymorphic = Maybe_polymorphic.is_polymorphic maybe_polymorphic }
    , prefix )
  ;;

  let get_row t maybe_polymorphic which txt =
    let name = name t which in
    let value =
      match which with
      | Derived_on | Set ->
        Option.map t.diff ~f:(fun diff ->
          Row_diff.derived_on_structure diff maybe_polymorphic txt ~for_diff:false)
      | Diff -> Some (Text (Prefix.to_string txt))
    in
    Variant_row
      { name; polymorphic = Maybe_polymorphic.is_polymorphic maybe_polymorphic; value }
  ;;

  let function_name t txt =
    Build_helper.Text
      (Function_name.to_string txt ^ "_" ^ Variant_row_name.to_string t.name)
  ;;
end

(* For types such as

   1. [type t = Foo]
   2. [type t = Foo of int]

   it's not possible for [get] to return the [Set] case

   For [1] the diff will always be empty
   For [2] if it's not empty it'll always be [Diff_foo]
*)
let can_get_set_case rows =
  match rows with
  | [] | [ _ ] -> false
  | _ :: _ :: _ -> true
;;

(* In the above examples, the diff types will be

   1. [type t = Set_foo] (Although it will be unused)
   2. [type t = Diff_foo of Diff_of_int.t]

   So for [1] we still want to define the set case, but we'll never return it in [get]

   (We could also make the diff type something like [Nothing.t], but this seems easier.)
*)
let define_set_case rows row = can_get_set_case rows || Option.is_none (Row.diff row)

let diff_type
  (type row_type diff_type prefix)
  rows
  (maybe_polymorphic : (row_type, diff_type, prefix) Maybe_polymorphic.t)
  : diff_type
  =
  let set_rows, diff_rows =
    List.map rows ~f:(fun row ->
      let diff, derived_on_type =
        match Row.diff row with
        | None -> None, None
        | Some row_diff ->
          Some (Row.Row_diff.core_diff row_diff), Some (Row.Row_diff.row_type row_diff)
      in
      ( Option.some_if (define_set_case rows row) (Row.name row Set, derived_on_type)
      , Option.map diff ~f:(fun diff ->
          let (diff_row_type : row_type) =
            let core = Type_kind.map_core (diff.diff_type, ()) ~f:(fun _ -> None) in
            match maybe_polymorphic with
            | Polymorphic -> core
            | Not_polymorphic -> Single core
          in
          Row.name row Diff, Some diff_row_type) ))
    |> List.unzip
  in
  let rows = List.filter_opt (set_rows @ diff_rows) in
  let map_rows rows ~f = List.map rows ~f:(fun (name, x) -> name, Option.map ~f x) in
  match maybe_polymorphic with
  | Polymorphic ->
    Type_kind.Polymorphic_variant (map_rows rows ~f:(Type_kind.map_core ~f:(fun _ -> ())))
  | Not_polymorphic ->
    Type_kind.Variant
      { rows = map_rows rows ~f:(Type_kind.map_variant_row_type ~f:(fun _ -> ()))
      ; equal_to = None
      }
;;

let get ~builder ~rows ~maybe_polymorphic:mp =
  let open (val builder : Builder.S) in
  (* match from, to_ with *)
  let diff_case row =
    case
      ~lhs:
        (* Could be:
           | A, A
           | B from, B to_
           | T (from1, from2, ...), T (to_1, to_2, ...)
           | R { field_a = from_a ; ... }, R { field_a = to_a ; ... }
        *)
        [%pat?
          ( [%p Row.get_row row mp Derived_on Prefix.from |> p]
          , [%p Row.get_row row mp Derived_on Prefix.to_ |> p] )]
      ~guard:None
      ~rhs:
        (match Row.diff row with
         | None ->
           (* The | A, A case *)
           [%expr Optional_diff.none]
         | Some diff ->
           (* B: d = get_B ~from ~to_
              T: d = get_T ~from:(local_ ({ Gel.g = from1 }, ...)) ~to_:...
              R: d = get_R ~from:(local_ { field_a = from_a;...}) ~to_:...
           *)
           let d =
             [%expr
               [%e Row.function_name row Function_name.get |> e]
                 ~from:[%e Row.Row_diff.derived_on diff mp Prefix.from |> e]
                 ~to_:[%e Row.Row_diff.derived_on diff mp Prefix.to_ |> e]]
           in
           [%expr
             Optional_diff.map [%e d] ~f:(fun diff ->
               (* Diff_B diff | Diff_T diff | Diff_R diff *)
               [%e Row.get_row row mp Diff Prefix.diff |> e])])
  in
  let set_case row =
    if not (can_get_set_case rows)
    then None
    else
      Some
        (case
           ~lhs:
             (* _, B to_
                _, T (to_1, to_2, ...)
                _, R { field_a = to_a ; field_b = to_b ;  ... }
             *)
             [%pat? _, [%p Row.get_row row mp Derived_on Prefix.to_ |> p]]
           ~guard:None
           ~rhs:
             (* Set_B to_
                Set_T (to_1, to_2)
                Set_R { field_a = to_a ; field_b = to_b ; ... } *)
             [%expr Optional_diff.return [%e Row.get_row row mp Set Prefix.to_ |> e]])
  in
  List.fold
    rows
    ~f:(fun expr row ->
      match Row.diff row with
      | None -> expr
      | Some row_diff ->
        (* Pre-allocate all [get] functions to avoid allocating later.
           [test_variant.ml:"inlined tuples and records - parametrized"] fails without
           this *)
        let core_diff = Row.Row_diff.core_diff row_diff in
        [%expr
          let [%p Row.function_name row Function_name.get |> p] =
            [%e core_diff.functions.get]
          in
          [%e expr]])
    ~init:
      [%expr
        fun ~from ~to_ ->
          if Base.phys_equal from to_
          then Optional_diff.none
          else
            [%e
              pexp_match
                [%expr from, to_]
                (List.map rows ~f:diff_case @ List.filter_map rows ~f:set_case)]]
;;

let name_expr row which ~builder =
  let open (val builder : Builder.S) in
  pexp_constant
    (Pconst_string (Row.name row which |> Variant_row_name.to_string, loc, None))
;;

let apply_exn ~rows ~builder ~maybe_polymorphic:mp =
  let open (val builder : Builder.S) in
  let set_cases =
    List.filter_map rows ~f:(fun row ->
      if not (define_set_case rows row)
      then None
      else
        Some
          (case
             ~lhs:
               (* _, Set_A
                  _, Set_B to_
                  _, Set_T (to_1, to_2, ...)
                  _, Set_R { field_a = to_a ; field_b = to_b ; ... }
               *)
               [%pat? _, [%p Row.get_row row mp Set Prefix.to_ |> p]]
             ~guard:None
             ~rhs:
               (* A
                  B to_
                  T (to_1, to_2, ...)
                  R {field_a = to_a ; field_b = to_b ; ... }
               *)
               (Row.get_row row mp Derived_on Prefix.to_ |> e)))
  in
  let diff_cases =
    List.filter_map rows ~f:(fun row ->
      match Row.diff row with
      | None -> None
      | Some type_ ->
        Some
          (case
             ~lhs:
               (* | B derived_on, Diff_B diff
                  | T (derived_on1, derived_on2, ...), Diff_T diff
                  | R { field_a = derived_on_a ; ... }, Diff_R diff
               *)
               [%pat?
                 ( [%p Row.get_row row mp Derived_on Prefix.derived_on |> p]
                 , [%p Row.get_row row mp Diff Prefix.diff |> p] )]
             ~guard:None
             ~rhs:
               [%expr
                 (* {[let to_b = apply_b derived_on diff in to_b]}
                     OR
                     {[let ({ Gel.g = to_1 }, { Gel.g = to_2 }, ...) =
                         apply_t (local_ ({ Gel.g = derived_on1 } , { Gel.g = derived_on2 }, ....)) diff
                       in
                       T (to_1, to_2, ...)
                     ]}
                     OR
                     {[let { R_record.field_a = to_a ; ... } =
                         apply_r (local_ { R_record.field_a = derived_on_a ; ... }) diff
                       in
                       R { field_a = to_a ; ... }]}
                  *)
                 let [%p Row.Row_diff.derived_on type_ mp Prefix.to_ |> p] =
                   [%e Row.function_name row Function_name.apply_exn |> e]
                     [%e Row.Row_diff.derived_on type_ mp Prefix.derived_on |> e]
                     diff
                 in
                 [%e Row.get_row row mp Derived_on Prefix.to_ |> e]]))
  in
  let name_expr = name_expr ~builder in
  let error_cases =
    List.filter_map rows ~f:(fun row ->
      match Row.diff row with
      | None -> None
      | Some _ ->
        (match rows with
         | [] | [ _ ] -> None
         | _ :: _ :: _ ->
           case
             ~lhs:
               [%pat?
                 derived_on, [%p Row.get_row row mp Diff (Prefix.of_string "_d") |> p]]
             ~guard:None
             ~rhs:
               [%expr
                 failwith
                   ("Diff mismatch. Applying to variant "
                    ^ derived_on_variant_name derived_on
                    ^ " but diff is of variant "
                    ^ [%e name_expr row Diff])]
           |> Option.return))
  in
  let derived_on_variant_name variant =
    pexp_match
      variant
      (List.map rows ~f:(fun row ->
         case
           ~lhs:(Row.get_row row mp Derived_on (Prefix.of_string "_x") |> p)
           ~guard:None
           ~rhs:(name_expr row Derived_on)))
  in
  let function_ =
    [%expr
      fun derived_on diff ->
        [%e pexp_match [%expr derived_on, diff] (set_cases @ diff_cases @ error_cases)]]
  in
  let init =
    if List.is_empty error_cases
    then function_
    else
      [%expr
        let derived_on_variant_name derived_on =
          [%e derived_on_variant_name [%expr derived_on]]
        in
        [%e function_]]
  in
  List.fold
    rows
    ~f:(fun expr row ->
      (* Pre-allocate all the [apply] functions *)
      match Row.diff row with
      | None -> expr
      | Some row_diff ->
        let core_diff = Row.Row_diff.core_diff row_diff in
        [%expr
          let [%p Row.function_name row Function_name.apply_exn |> p] =
            [%e core_diff.functions.apply_exn]
          in
          [%e expr]])
    ~init
;;

let of_list ~rows ~maybe_polymorphic:mp ~builder =
  let open (val builder : Builder.S) in
  let name_expr = name_expr ~builder in
  let case = case ~guard:None in
  let set_rows = List.filter rows ~f:(define_set_case rows) in
  let diff_rows =
    List.filter_map rows ~f:(fun row ->
      Option.map (Row.diff row) ~f:(fun diff -> row, diff))
  in
  let prefix = Prefix.of_string in
  let any = prefix "_" in
  let diff_variant_name variant =
    (* match variant with
       | Set_to_a -> "Set_to_a"
       | Diff_b _ -> "Diff_b"
       ...
    *)
    let case which row =
      case ~lhs:(Row.get_row row mp which any |> p) ~rhs:(name_expr row which)
    in
    pexp_match
      variant
      (List.map set_rows ~f:(case Set)
       @ List.map diff_rows ~f:(fun (row, _diff) -> case Diff row))
  in
  let r row which prefix = Row.get_row row mp which prefix in
  let error_case ?(local = false) row which =
    let error =
      [%expr
        failwith
          ("Diff mismatch. Can't combine diff of variant "
           ^ [%e name_expr row which]
           ^ " with diff of variant "
           ^ diff_variant_name t)]
    in
    case ~lhs:[%pat? t] ~rhs:(if local then [%expr [%e error]] else error)
  in
  let unpack_diffs row ~for_:which =
    (* Get all [Diff]s of a given variant:

       function
       | Diff_b d -> d
       | _ -> mismatch error

       [for_] is just used in the error message to indicate we tried to combine non-matching variants

       If there are no diffs of a given variant (e.g. if the variant is just [A], not [A
       of ...]), there is only the error case.

       If there is only one row, there are no error cases.
    *)
    pexp_function
      ([ Option.some_if
           (Option.is_some (Row.diff row))
           (case ~lhs:[%pat? [%p r row Diff Prefix.diff |> p]] ~rhs:[%expr diff])
       ; Option.some_if (List.length rows > 1) (error_case row which)
       ]
       |> List.filter_opt)
  in
  let diffs_only_case row =
    (* All ts are of a [Diff] variant, no [Set] variant *)
    case
      ~lhs:[%pat? [], [%p r row Diff (Prefix.of_string "hd") |> p] :: tl]
      ~rhs:
        [%expr
          let tl = Base.List.map tl ~f:[%e unpack_diffs row ~for_:Diff] in
          let d = [%e Row.function_name row Function_name.of_list_exn |> e] (hd :: tl) in
          Optional_diff.map d ~f:(fun diff -> [%e r row Diff Prefix.diff |> e])]
  in
  let set_followed_by_diffs_case row =
    (* We can ignore everything before the last [Set], so we just look at a single [Set]
       followed by a bunch of [Diffs] *)
    case
      ~lhs:[%pat? [%p r row Set Prefix.to_ |> p] :: _, diffs]
      ~rhs:
        (let unpack_diffs = unpack_diffs row ~for_:Set in
         match Row.diff row with
         | None ->
           (* The original row is just [A], not [A of ...], so there is no [Diff_a]. That
              means following [Set_to_A] with any diff is an error *)
           [%expr
             let () = Base.List.iter diffs ~f:[%e unpack_diffs] in
             Optional_diff.return [%e r row Set Prefix.to_ |> e]]
         | Some type_ ->
           let derived_on ?skip_local () =
             Row.Row_diff.derived_on type_ mp ?skip_local Prefix.to_
           in
           (* List.fold (unpack_diffs diffs) ~init:to_ ~f:(fun acc diff ->
              apply_exn_B acc diff)
           *)
           [%expr
             let diffs = Base.List.map diffs ~f:[%e unpack_diffs] in
             let [%p derived_on () |> p] =
               Base.List.fold
                 ~init:[%e derived_on ~skip_local:true () |> e]
                 diffs
                 ~f:(fun acc diff ->
                   let [%p derived_on () |> p] =
                     [%e Row.function_name row Function_name.apply_exn |> e] acc diff
                   in
                   [%e derived_on ~skip_local:true () |> e])
             in
             Optional_diff.return [%e r row Set Prefix.to_ |> e]])
  in
  let expr =
    if List.is_empty diff_rows
    then
      (* All diffs are of the [Set_to] variant, so just pick the last one *)
      [%expr
        function
        | [] -> Optional_diff.none
        | _ :: _ as l -> Optional_diff.return (Base.List.last_exn l)]
    else
      [%expr
        function
        | [] -> Optional_diff.none
        | [ hd ] -> Optional_diff.return hd
        | l ->
          (* Otherwise look at the last [Set_to] (if any) + any diffs after it *)
          let diffs_rev, rest_rev =
            Base.List.rev l
            |> Base.List.split_while
                 ~f:
                   [%e
                     let case row which return =
                       case ~lhs:[%pat? [%p r row which any |> p]] ~rhs:return
                     in
                     pexp_function
                       (List.map diff_rows ~f:(fun (row, _) -> case row Diff [%expr true])
                        @ List.map set_rows ~f:(fun row -> case row Set [%expr false]))]
          in
          let diffs = Base.List.rev diffs_rev in
          [%e
            let any_row rows which =
              List.map rows ~f:(fun row -> r row which any |> p)
              |> List.reduce_exn ~f:ppat_or
            in
            pexp_match
              [%expr rest_rev, diffs]
              ([ case ~lhs:[%pat? [], []] ~rhs:[%expr assert false] ]
               @ (if List.is_empty diff_rows
                  then []
                  else
                    [ case (* the first elt in [rest_rev] can't be a [Diff _]  *)
                        ~lhs:[%pat? [%p any_row (List.map diff_rows ~f:fst) Diff] :: _, _]
                        ~rhs:[%expr assert false]
                    ])
               @ (if List.is_empty set_rows
                  then []
                  else
                    [ case (* [diffs] don't contain a [Set _]  *)
                        ~lhs:[%pat? _, [%p any_row set_rows Set] :: _]
                        ~rhs:[%expr assert false]
                    ; case (* [Set _] followed by no diffs, just return the [Set _] *)
                        ~lhs:[%pat? ([%p any_row set_rows Set] as t) :: _, []]
                        ~rhs:[%expr Optional_diff.return t]
                    ])
               @ List.map diff_rows ~f:(fun (row, _) -> diffs_only_case row)
               @ List.map set_rows ~f:set_followed_by_diffs_case)]]
  in
  let init =
    (* The diff_variant_name function is only used in error cases *)
    if List.is_empty diff_rows || List.length rows = 1
    then expr
    else
      [%expr
        let diff_variant_name diff = [%e diff_variant_name [%expr diff]] in
        [%e expr]]
  in
  List.fold
    diff_rows
    ~f:(fun expr (row, diff) ->
      (* Pre-allocate all the [apply/of_list] functions *)
      let core_diff = Row.Row_diff.core_diff diff in
      let expr =
        if define_set_case rows row
        then
          [%expr
            let [%p Row.function_name row Function_name.apply_exn |> p] =
              [%e core_diff.functions.apply_exn]
            in
            [%e expr]]
        else expr
      in
      [%expr
        let [%p Row.function_name row Function_name.of_list_exn |> p] =
          [%e core_diff.functions.of_list_exn]
        in
        [%e expr]])
    ~init
;;

let create_maybe_polymorphic
  (type row_type diff_type prefix)
  (maybe_polymorphic : (row_type, diff_type, prefix) Maybe_polymorphic.t)
  ~(rows : (Variant_row_name.t * row_type option) list)
  ~context
  ~create_core
  : diff_type * prefix list * Diff.Functions.t
  =
  let { Context.builder; _ } = context in
  let open (val builder : Builder.S) in
  let module_name_generator = Module_name_generator.create maybe_polymorphic rows in
  let rows, prefix =
    List.map rows ~f:(fun (variant_name, variant_type) ->
      Row.create
        variant_name
        variant_type
        maybe_polymorphic
        ~context
        ~create_core
        ~module_name_generator)
    |> List.unzip
  in
  let get = get ~builder ~rows ~maybe_polymorphic in
  let apply_exn = apply_exn ~builder ~rows ~maybe_polymorphic in
  let of_list_exn = of_list ~builder ~rows ~maybe_polymorphic in
  let functions = { Diff.Functions.get; apply_exn; of_list_exn } in
  diff_type rows maybe_polymorphic, List.filter_opt prefix, functions
;;

let create rows ~context ~create_core =
  let diff_type, prefix, functions =
    create_maybe_polymorphic Not_polymorphic ~rows ~context ~create_core
  in
  { Diff.prefix = Items.concat prefix
  ; diff_type = This { kind = diff_type; nonrec_ = false; unboxed_override = None }
  ; functions = Ok functions
  }
;;

let create_polymorphic rows ~context ~create_core =
  let diff_type, prefix, functions =
    create_maybe_polymorphic Polymorphic ~rows ~context ~create_core
  in
  let (_ : unit list) = prefix in
  { Core_diff.diff_type; functions }
;;
