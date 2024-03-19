open! Base
open Ppxlib

type 'extra core_kind =
  | Var of Var.t
  | Tuple of 'extra core list
  | Constr of 'extra constr
  | Polymorphic_variant of 'extra polymorphic_variant

and 'extra core = 'extra core_kind * 'extra

and 'extra constr =
  { params : 'extra core list
  ; module_ : Module_name.t Longident_helper.t option
  ; type_name : Type_name.t
  }

and 'extra polymorphic_variant = (Variant_row_name.t * 'extra core option) list

type 'extra record_field =
  { field_name : Record_field_name.t
  ; field_type : 'extra core
  ; mutable_ : bool
  ; global : bool
  }

type 'extra variant_row_type =
  | Single of 'extra core
  | Inlined_record of 'extra record_field list
  | Inlined_tuple of 'extra core list

type 'extra variant = (Variant_row_name.t * 'extra variant_row_type option) list

type 'extra t =
  | Abstract
  | Core of 'extra core
  | Variant of
      { equal_to : 'extra core option
      ; rows : 'extra variant
      }
  | Record of
      { equal_to : 'extra core option
      ; fields : 'extra record_field list
      ; local : bool
      }

let rec core_to_ppx (core : unit core) ~builder =
  let core_kind, () = core in
  let open (val builder : Builder.S) in
  match core_kind with
  | Var v -> Var.core_type v ~builder
  | Tuple l -> ptyp_tuple (List.map l ~f:(fun t -> core_to_ppx t ~builder))
  | Constr { params; module_; type_name } ->
    let lident_helper =
      module_
      |> Option.map ~f:(Longident_helper.map ~f:Module_name.to_string)
      |> Longident_helper.add_suffix ~suffix:(Type_name.to_string type_name, [])
    in
    ptyp_constr
      (Located.mk (Longident_helper.to_longident lident_helper))
      (List.map params ~f:(fun t -> core_to_ppx t ~builder))
  | Polymorphic_variant variants ->
    ptyp_variant
      (List.map variants ~f:(fun (variant_name, variant_type) ->
         let bool, types =
           match variant_type with
           | None -> true, []
           | Some core -> false, [ core_to_ppx core ~builder ]
         in
         rtag (Located.mk (Variant_row_name.to_string variant_name)) bool types))
      Closed
      None
;;

let label_declarations record_fields ~builder =
  let open (val builder : Builder.S) in
  List.map record_fields ~f:(fun { field_name; field_type; mutable_; global } ->
    let declaration =
      label_declaration
        ~name:(Located.mk (Record_field_name.to_string field_name))
        ~type_:(core_to_ppx field_type ~builder)
        ~mutable_:(if mutable_ then Mutable else Immutable)
    in
    let modality = if global then Some Global else None in
    modality, declaration)
;;

let to_ppx_kind t ~builder =
  let open (val builder : Builder.S) in
  match t with
  | Abstract -> Ptype_abstract, None
  | Core core -> Ptype_abstract, Some (core_to_ppx core ~builder)
  | Record { fields; local = (_ : bool); equal_to } ->
    ( ptype_record (label_declarations fields ~builder)
    , Option.map equal_to ~f:(core_to_ppx ~builder) )
  | Variant { rows; equal_to } ->
    ( Ptype_variant
        (List.map rows ~f:(fun (row_name, row_type) ->
           constructor_declaration
             ~name:(Located.mk (Variant_row_name.to_string row_name))
             ~res:None
             ~args:
               (match row_type with
                | None -> Pcstr_tuple []
                | Some (Single type_) -> Pcstr_tuple [ core_to_ppx type_ ~builder ]
                | Some (Inlined_tuple l) ->
                  Pcstr_tuple (List.map l ~f:(fun t -> core_to_ppx t ~builder))
                | Some (Inlined_record fields) ->
                  pcstr_record (label_declarations fields ~builder))))
    , Option.map equal_to ~f:(core_to_ppx ~builder) )
;;

let rec fold_core (type extra) core ~init ~f =
  let core, (_ : extra) = core in
  let init = f init core in
  match core with
  | Var (_ : Var.t) -> init
  | Tuple l -> List.fold l ~init ~f:(fun acc core -> fold_core core ~init:acc ~f)
  | Constr
      { params
      ; module_ = (_ : Module_name.t Longident_helper.t option)
      ; type_name = (_ : Type_name.t)
      } -> List.fold params ~init ~f:(fun acc core -> fold_core core ~init:acc ~f)
  | Polymorphic_variant variant ->
    List.fold variant ~init ~f:(fun acc ((_ : Variant_row_name.t), row_type) ->
      match row_type with
      | None -> acc
      | Some core -> fold_core core ~init:acc ~f)
;;

let fold_record_fields l ~init ~f =
  List.fold
    l
    ~init
    ~f:
      (fun
        acc
        { field_name = (_ : Record_field_name.t)
        ; field_type
        ; mutable_ = (_ : bool)
        ; global = (_ : bool)
        }
        -> fold_core field_type ~init:acc ~f)
;;

let fold t ~init ~f =
  match t with
  | Abstract -> init
  | Core core -> fold_core core ~init ~f
  | Record { fields; local = (_ : bool); equal_to } ->
    fold_record_fields
      fields
      ~f
      ~init:
        (match equal_to with
         | None -> init
         | Some core -> fold_core core ~init ~f)
  | Variant { rows; equal_to } ->
    List.fold
      rows
      ~f:(fun acc ((_ : Variant_row_name.t), row_type) ->
        match row_type with
        | None -> acc
        | Some (Single core) -> fold_core core ~init:acc ~f
        | Some (Inlined_record fields) -> fold_record_fields fields ~init:acc ~f
        | Some (Inlined_tuple l) ->
          List.fold l ~init:acc ~f:(fun acc t -> fold_core t ~init:acc ~f))
      ~init:
        (match equal_to with
         | None -> init
         | Some core -> fold_core core ~init ~f)
;;

let vars t =
  fold t ~init:[] ~f:(fun acc -> function
    | Var var -> var :: acc
    | Tuple _ | Constr _ | Polymorphic_variant _ -> acc)
  |> List.rev
;;

let constrs t =
  fold t ~init:[] ~f:(fun acc -> function
    | Constr constr -> constr :: acc
    | Var _ | Tuple _ | Polymorphic_variant _ -> acc)
  |> List.rev
;;

let is_local = function
  | Abstract | Core _ | Variant _ -> false
  | Record { local; _ } -> local
;;

let rec map_core (core_kind, extra) ~f = map_core_kind core_kind ~f, f extra

and map_core_kind kind ~f =
  let map_core = map_core ~f in
  match kind with
  | Var var -> Var var
  | Tuple l -> Tuple (List.map l ~f:map_core)
  | Constr { params; module_; type_name } ->
    Constr { params = List.map params ~f:map_core; module_; type_name }
  | Polymorphic_variant l ->
    Polymorphic_variant
      (List.map l ~f:(fun (name, maybe_core) -> name, Option.map maybe_core ~f:map_core))
;;

let map_record fields ~f =
  List.map fields ~f:(fun { field_name; field_type; global; mutable_ } ->
    { field_name; field_type = map_core field_type ~f; global; mutable_ })
;;

let map_variant_row_type row_type ~f =
  match row_type with
  | Single core -> Single (map_core core ~f)
  | Inlined_tuple l -> Inlined_tuple (List.map l ~f:(map_core ~f))
  | Inlined_record r -> Inlined_record (map_record r ~f)
;;

let map_variant ~f =
  List.map ~f:(fun (name, maybe_type) ->
    name, Option.map maybe_type ~f:(map_variant_row_type ~f))
;;

let map t ~f =
  match t with
  | Abstract -> Abstract
  | Core core -> Core (map_core core ~f)
  | Record { fields; local; equal_to } ->
    Record
      { fields = map_record fields ~f
      ; local
      ; equal_to = Option.map ~f:(map_core ~f) equal_to
      }
  | Variant { rows; equal_to } ->
    Variant
      { rows = map_variant rows ~f; equal_to = Option.map ~f:(map_core ~f) equal_to }
;;

let not_supported builder s =
  let open (val builder : Builder.S) in
  raise_error (Printf.sprintf "%s not_supported" s)
;;

let duplicate_how_to_diff how_to_diff1 how_to_diff2 ~builder =
  let open (val builder : Builder.S) in
  let s1 = How_to_diff.Custom.to_string how_to_diff1 in
  let s2 = How_to_diff.Custom.to_string how_to_diff2 in
  if String.( = ) s1 s2
  then raise_error (Printf.sprintf "duplicate how to diff: \"%s\"" s1)
  else
    raise_error
      (Printf.sprintf "cannot use both \"%s\" and \"%s\" on the same type" s1 s2)
;;

let rec create_core core_type ~builder : How_to_diff.t core =
  let how_to_diff = How_to_diff.Custom.of_core_type core_type ~builder in
  let kind : How_to_diff.t core_kind =
    match core_type.ptyp_desc with
    | Ptyp_var var -> Var (Var.of_string var)
    | Ptyp_tuple types -> Tuple (List.map types ~f:(create_core ~builder))
    | Ptyp_constr (id, core_types) ->
      let open (val builder : Builder.S) in
      let longident_helper = Longident_helper.of_longident id.txt ~builder in
      let module_, type_name =
        match longident_helper with
        | Simple l ->
          ( Longident_helper.of_simple_list (Nonempty_list.drop_last l)
          , Nonempty_list.last l )
        | Functor_application (functor_, arg, rest) ->
          (match rest with
           | [] -> raise_error "Expected a type, got functor application"
           | _ :: _ ->
             let module_ : string Longident_helper.t =
               Functor_application (functor_, arg, List.drop_last_exn rest)
             in
             Some module_, List.last_exn rest)
      in
      Constr
        { params = List.map core_types ~f:(create_core ~builder)
        ; module_ = Option.map module_ ~f:(Longident_helper.map ~f:Module_name.of_string)
        ; type_name = Type_name.of_string type_name
        }
    | Ptyp_variant (rows, closed_flag, labels) ->
      (match labels with
       | None -> ()
       | Some _ -> not_supported builder "Polymophic variants with labels");
      (match closed_flag with
       | Closed -> ()
       | Open -> not_supported builder "Open polymorphic variants");
      Polymorphic_variant
        (List.map rows ~f:(fun (row : row_field) ->
           let { prf_desc; prf_loc = _; prf_attributes = _ } = row in
           match prf_desc with
           | Rinherit _ -> not_supported builder "Rinherit"
           | Rtag (label, bool, types) ->
             let variant_name = label.txt in
             let variant_type =
               match bool, types with
               | true, [] -> None
               | false, [ core_type ] ->
                 let variant_type =
                   let kind, how_to_diff = create_core core_type ~builder in
                   let how_to_diff =
                     Option.merge
                       how_to_diff
                       (How_to_diff.Custom.of_rtag row ~builder)
                       ~f:(duplicate_how_to_diff ~builder)
                   in
                   kind, how_to_diff
                 in
                 Some variant_type
               | false, [] | true, _ :: _ ->
                 not_supported builder "Unknown polymorphic variant"
               | _, _ :: _ :: _ -> not_supported builder "Multi-type polymorphic variant"
             in
             Variant_row_name.of_string variant_name, variant_type))
    | Ptyp_any -> not_supported builder "Ptyp_any"
    | Ptyp_arrow _ -> not_supported builder "Ptyp_arrow"
    | Ptyp_object _ -> not_supported builder "Ptyp_object"
    | Ptyp_class _ -> not_supported builder "Ptyp_class"
    | Ptyp_alias _ -> not_supported builder "Ptyp_alias"
    | Ptyp_poly _ -> not_supported builder "Ptyp_poly"
    | Ptyp_package _ -> not_supported builder "Ptyp_package"
    | Ptyp_extension _ -> not_supported builder "Ptyp_extension"
  in
  kind, how_to_diff
;;

let core_of_ppx = create_core

let create_record fields ~builder =
  let open (val builder : Builder.S) in
  List.map fields ~f:(fun (field : label_declaration) ->
    let modality, field = get_label_declaration_modality field in
    let global =
      match modality with
      | Some Global -> true
      | None -> false
    in
    let { pld_name; pld_mutable; pld_type; pld_loc = _; pld_attributes = _ } = field in
    let field_type =
      let kind, how_to_diff = create_core pld_type ~builder in
      let how_to_diff =
        Option.merge
          how_to_diff
          (How_to_diff.Custom.of_label_declaration field ~builder)
          ~f:(duplicate_how_to_diff ~builder)
      in
      kind, how_to_diff
    in
    { field_name = Record_field_name.of_string pld_name.txt
    ; field_type
    ; mutable_ =
        (match pld_mutable with
         | Mutable -> true
         | Immutable -> false)
    ; global
    })
;;

let of_ppx_kind
  ((type_kind : type_kind), (core_type : core_type option))
  ~how_to_diff
  ~builder
  =
  let t =
    match type_kind, core_type with
    | Ptype_abstract, None -> Abstract
    | Ptype_abstract, Some core_type -> Core (create_core core_type ~builder)
    | Ptype_record fields, equal_to ->
      Record
        { fields = create_record fields ~builder
        ; local = false
        ; equal_to = Option.map equal_to ~f:(create_core ~builder)
        }
    | Ptype_variant rows, equal_to ->
      Variant
        { rows =
            List.map rows ~f:(fun (variant : constructor_declaration) ->
              let open (val builder : Builder.S) in
              let { pcd_name
                  ; pcd_vars
                  ; pcd_args
                  ; pcd_res
                  ; pcd_loc = _
                  ; pcd_attributes = _
                  }
                =
                variant
              in
              (match pcd_res with
               | None -> ()
               | Some _ -> not_supported builder "[pcd_res = Some _] not supported");
              (match pcd_vars with
               | [] -> ()
               | _ :: _ -> not_supported builder "only empty pcd_vars supported");
              let how_to_diff =
                How_to_diff.Custom.of_constructor_declaration variant ~builder
              in
              let error_if_custom_how_to_diff msg =
                match how_to_diff with
                | None -> ()
                | Some how_to_diff ->
                  raise_error
                    (Printf.sprintf
                       "%s attributes, e.g. %s not supported for %s"
                       Shared.name_of_ppx
                       (How_to_diff.Custom.to_attribute_string how_to_diff)
                       msg)
              in
              let variant_type =
                match pcd_args with
                | Pcstr_tuple [] -> None
                | Pcstr_tuple [ core_type ] ->
                  let kind, core_how_to_diff = create_core core_type ~builder in
                  let how_to_diff =
                    Option.merge
                      core_how_to_diff
                      how_to_diff
                      ~f:(duplicate_how_to_diff ~builder)
                  in
                  Some (Single (kind, how_to_diff))
                | Pcstr_record record ->
                  error_if_custom_how_to_diff "inlined records";
                  Some (Inlined_record (create_record record ~builder))
                | Pcstr_tuple types ->
                  error_if_custom_how_to_diff "inlined tuples";
                  Some (Inlined_tuple (List.map types ~f:(create_core ~builder)))
              in
              Variant_row_name.of_string pcd_name.txt, variant_type)
        ; equal_to = Option.map equal_to ~f:(create_core ~builder)
        }
    | Ptype_open, _ -> not_supported builder "Ptype_open"
  in
  match (how_to_diff : How_to_diff.Maybe_abstract.t) with
  | None -> t, None
  | Some Abstract ->
    let (_ : unit t) =
      let open (val builder : Builder.S) in
      map t ~f:(function
        | None -> ()
        | Some type_how_to_diff ->
          raise_error
            (Printf.sprintf
               "%s will be ignored because it is inside a type already marked %s"
               (How_to_diff.Custom.to_attribute_string type_how_to_diff)
               (How_to_diff.Custom.Or_abstract.to_string Abstract)))
    in
    Abstract, None
  | Some (Custom (Atomic atomic)) -> t, Some atomic
  | Some (Custom ((As_set _ | As_map _) as how_to_diff)) ->
    let error type_kind =
      let open (val builder : Builder.S) in
      raise_error
        (Printf.sprintf
           "\"%s\" can't be used with %s types"
           (How_to_diff.Custom.to_string how_to_diff)
           type_kind)
    in
    (match t with
     | Core (kind, core_how_to_diff) ->
       (match core_how_to_diff with
        | None -> Core (kind, Some how_to_diff), None
        | Some core_how_to_diff ->
          duplicate_how_to_diff ~builder core_how_to_diff how_to_diff)
     | Abstract -> error "abstract"
     | Record _ -> error "record"
     | Variant _ -> error "variant")
;;

let can_be_unboxed = function
  | Abstract | Core _ -> false
  | Variant { equal_to = _; rows = [ (_, Some _) ] } -> true
  | Variant _ -> false
  | Record { equal_to = _; fields = [ _ ]; local = _ } -> true
  | Record _ -> false
;;
