open Base
open Ppxlib

module Functions = struct
  type t =
    { get : expression
    ; apply_exn : expression
    ; of_list_exn : expression
    }

  let to_items
    t_or_error
    ~derived_on_type_declaration
    ~diff_type_declaration
    ~builder
    ~derived_on_type_is_local
    =
    let params = derived_on_type_declaration.Type_declaration.params in
    let vars = List.map params ~f:Type_param.var in
    let open (val builder : Builder.S) in
    let pointer type_declaration =
      type_declaration |> Type_declaration.pointer |> Type_kind.core_to_ppx ~builder
    in
    let get_base (derived_on_mode, derived_on) t =
      let arg label =
        { arg_label = Labelled label; arg_mode = derived_on_mode; arg_type = derived_on }
      in
      tarrow
        [ arg "from"; arg "to_" ]
        { result_mode = Some Local; result_type = [%type: [%t t] Optional_diff.t] }
    in
    let apply_base (derived_on_mode, derived_on) t =
      tarrow
        [ { arg_label = Nolabel; arg_mode = derived_on_mode; arg_type = derived_on }
        ; { arg_label = Nolabel; arg_mode = None; arg_type = t }
        ]
        { result_mode = derived_on_mode; result_type = derived_on }
    in
    let of_list_base _derived_on t =
      tarrow
        [ { arg_label = Nolabel; arg_mode = None; arg_type = [%type: [%t t] list] } ]
        { result_mode = Some Local; result_type = [%type: [%t t] Optional_diff.t] }
    in
    let fun_type base ~var_functions =
      let v = Var.core_type ~builder in
      let derived_on_mode = if derived_on_type_is_local then Some Local else None in
      (* Generate the parametrized functions, e.g.
         (from:'a -> to_:'a -> local_ 'a_diff Optional_diff.t)
         (from:'b -> to_:'b -> local_ 'b_diff Optional_diff.t)
      *)
      tarrow_maybe
        (List.concat_map vars ~f:(fun var ->
           List.map var_functions ~f:(fun fn ->
             let arg_type = fn (None, v var) (v (Var.diff_var var)) in
             { arg_label = Nolabel; arg_mode = None; arg_type })))
        (base
           (derived_on_mode, pointer derived_on_type_declaration)
           (pointer diff_type_declaration))
    in
    let sig_items =
      [%sig:
        val get : [%t fun_type get_base ~var_functions:[ get_base ]]
        val apply_exn : [%t fun_type apply_base ~var_functions:[ apply_base ]]

        val of_list_exn
          : [%t fun_type of_list_base ~var_functions:[ of_list_base; apply_base ]]]
    in
    let struct_items =
      Result.map t_or_error ~f:(fun { get; apply_exn; of_list_exn } ->
        let fn ~names ~init () =
          let open (val builder : Builder.S) in
          let var_functions =
            List.concat_map vars ~f:(fun var ->
              List.map names ~f:(fun name -> Function_name.function_of_var name var))
          in
          (* [fun _get_a _get_b -> init] *)
          List.fold_right var_functions ~init ~f:(fun var_fn expr ->
            [%expr fun [%p p var_fn] -> [%e expr]])
        in
        [%str
          let get : [%t fun_type get_base ~var_functions:[ get_base ]] =
            [%e fn ~names:[ Function_name.get ] ~init:get ()]
          ;;

          let apply_exn : [%t fun_type apply_base ~var_functions:[ apply_base ]] =
            [%e fn ~names:[ Function_name.apply_exn ] ~init:apply_exn ()]
          ;;

          let of_list_exn
            : [%t fun_type of_list_base ~var_functions:[ of_list_base; apply_base ]]
            =
            [%e
              fn
                ~names:[ Function_name.of_list_exn; Function_name.apply_exn ]
                ~init:of_list_exn
                ()]
          ;;])
    in
    { Items.sig_items; struct_items }
  ;;
end

module Diff_type_kind = struct
  type t =
    | This of
        { kind : unit Type_kind.t
        ; nonrec_ : bool
        ; unboxed_override : bool option
        }
    | Sorted_list of
        { single_kind : (Variant_row_name.t * unit Type_kind.core) list
        ; single_module_name : Module_name.t
        }
end

type t =
  { prefix : Items.t
  ; diff_type : Diff_type_kind.t
  ; functions : Functions.t Or_error.t
  }

let to_items t ~context ~(type_to_diff_declaration : unit Type_declaration.t) =
  let { prefix; diff_type; functions } = t in
  let { Context.builder; what_to_derive; _ } = context in
  let open (val builder : Builder.S) in
  let core_to_ppx = Type_kind.core_to_ppx ~builder in
  let params = type_to_diff_declaration.Type_declaration.params in
  (* If we have [('a, 'b) derived_on], we'll have [('a, 'b, 'a_diff, 'b_diff) t] *)
  let diff_type_params =
    params @ List.map params ~f:(Type_param.map_var ~f:Var.diff_var)
  in
  let diff_type_vars = List.map diff_type_params ~f:Type_param.var in
  let diff_type_declaration kind ~unboxed =
    { Type_declaration.params = diff_type_params; name = Type_name.t; kind; unboxed }
  in
  let ( diff_type_declaration
      , diff_type_flags
      , additional_type
      , compare_functions
      , create_functions )
    =
    let module Flags = Type_declaration.Flags in
    match diff_type with
    | This { kind; nonrec_; unboxed_override } ->
      let flags = { Flags.private_ = false; nonrec_ } in
      let unboxed =
        match unboxed_override with
        | Some unboxed -> unboxed
        | None -> Type_kind.can_be_unboxed kind
      in
      diff_type_declaration kind ~unboxed, flags, Items.empty, Items.empty, Items.empty
    | Sorted_list { single_kind; single_module_name } ->
      (* type ('a, 'b, 'a_diff, 'b_diff) single = [single_kind]
      *)
      let single_type_declaration : unit Type_declaration.t =
        let kind =
          Type_kind.Variant
            { rows =
                List.map single_kind ~f:(fun (name, x) -> name, Some (Type_kind.Single x))
            ; equal_to = None
            }
        in
        { params = diff_type_params
        ; name = Type_name.t
        ; kind =
            Variant
              { rows =
                  List.map single_kind ~f:(fun (name, x) ->
                    name, Some (Type_kind.Single x))
              ; equal_to = None
              }
        ; unboxed = Type_kind.can_be_unboxed kind
        }
      in
      let single_module =
        [ Items.to_module
            (Type_declaration.to_items
               single_type_declaration
               ~context:
                 { context with
                   what_to_derive =
                     What_to_derive.add
                       context.what_to_derive
                       What_to_derive.Entry.variants
                 })
            ~module_name:single_module_name
            ~builder
        ; Items.
            { sig_items = []
            ; struct_items =
                Ok
                  [ pstr_open
                      (open_infos
                         ~expr:
                           (pmod_ident
                              (Located.mk
                                 (Longident_helper.to_longident
                                    (Simple (Module_name.to_string single_module_name, [])))))
                         ~override:Fresh)
                  ]
            }
        ]
        |> Items.concat
      in
      let single_type =
        Type_declaration.pointer single_type_declaration ~module_:[ single_module_name ]
      in
      (* type ('a, 'b, 'a_diff, 'b_diff) t = ('a, 'b, 'a_diff, 'b_diff) single list *)
      let list = Type_name.of_string "list" in
      let list_kind =
        ( Type_kind.Constr { params = [ single_type ]; module_ = None; type_name = list }
        , () )
      in
      let diff_type_declaration =
        diff_type_declaration (Type_kind.Core list_kind) ~unboxed:false
      in
      let diff_type_flags =
        { Flags.private_ = (* we rely on the order of items in the list *) true
        ; nonrec_ = false
        }
      in
      let t_ = diff_type_declaration |> Type_declaration.pointer |> core_to_ppx in
      let create_arg_name row_name =
        String.uncapitalize (Variant_row_name.to_string row_name)
      in
      let create_type =
        tarrow_maybe
          (List.map single_kind ~f:(fun (row_name, row_type) ->
             let arg_name = create_arg_name row_name in
             let arg_type = core_to_ppx row_type in
             { arg_label = Optional arg_name; arg_mode = None; arg_type }))
          [%type: unit -> [%t t_]]
      in
      let create_of_variants_type =
        tarrow_maybe
          (List.map single_kind ~f:(fun (row_name, row_type) ->
             let arg_name = create_arg_name row_name in
             let arg_type =
               [%type:
                 ([%t core_to_ppx row_type], [%t core_to_ppx single_type]) Of_variant.t]
             in
             { arg_label = Labelled arg_name; arg_mode = Some Local; arg_type }))
          t_
      in
      let create_arg_names =
        List.map single_kind ~f:(fun (row_name, _) -> create_arg_name row_name)
      in
      let variants txt =
        Longident_helper.to_expression
          (Simple (Module_name.to_string single_module_name, [ "Variants"; txt ]))
          ~builder
      in
      let create_fn ~row_diff ~args_are_optional option_or_optional_diff =
        let row row_name text =
          Build_helper.Variant_row
            { name = row_name; polymorphic = false; value = Some (Text text) }
        in
        let maybe_add row_name =
          match option_or_optional_diff with
          | `option ->
            [%expr
              match [%e row_diff row_name] with
              | None -> diff
              | Some d -> [%e e (row row_name "d")] :: diff]
          | `optional_diff ->
            [%expr
              let d = [%e row_diff row_name] in
              if Optional_diff.is_none d
              then diff
              else (
                let d = Optional_diff.unsafe_value d in
                [%e e (row row_name "d")] :: diff)]
        in
        let init =
          [%expr
            let diff = [] in
            [%e
              List.fold
                single_kind
                ~f:(fun acc (row_name, _) ->
                  [%expr
                    let diff = [%e maybe_add row_name] in
                    [%e acc]])
                ~init:[%expr diff]]]
        in
        List.fold_right
          create_arg_names
          ~f:(fun name acc ->
            pexp_fun
              (if args_are_optional then Optional name else Labelled name)
              None
              (p (Text name))
              acc)
          ~init:(if args_are_optional then [%expr fun () -> [%e init]] else init)
      in
      let variants_to_rank = variants "to_rank" in
      let compare_functions =
        let sig_items = [] in
        let struct_items =
          [%str
            let compare_rank t1 t2 =
              Base.Int.compare ([%e variants_to_rank] t1) ([%e variants_to_rank] t2)
            ;;

            let equal_rank t1 t2 =
              Base.Int.equal ([%e variants_to_rank] t1) ([%e variants_to_rank] t2)
            ;;]
        in
        { Items.sig_items; struct_items = Ok struct_items }
      in
      let create_functions =
        let sig_items =
          [%sig:
            val singleton : [%t core_to_ppx single_type] -> [%t t_]
            val create : [%t create_type]
            val create_of_variants : [%t create_of_variants_type]]
        in
        let create =
          [%str
            let singleton x = [ x ]

            let create =
              [%e
                create_fn `option ~args_are_optional:true ~row_diff:(fun row_name ->
                  e (Text (create_arg_name row_name)))]
            ;;

            let create_of_variants =
              [%e
                create_fn
                  `optional_diff
                  ~args_are_optional:false
                  ~row_diff:(fun row_name ->
                  let name = create_arg_name row_name in
                  [%expr [%e Text name |> e] [%e variants name]])]
            ;;]
        in
        let t_of_sexp =
          let function_name = Function_name.of_string "of_sexp" in
          let var_functions =
            List.map diff_type_vars ~f:(Function_name.function_of_var function_name)
          in
          let init =
            [%expr
              fun sexp ->
                let l =
                  [%e
                    pexp_apply
                      [%expr t_of_sexp]
                      (List.map var_functions ~f:(fun fn -> Nolabel, e fn)
                       @ [ Nolabel, [%expr sexp] ])]
                  |> Base.List.sort ~compare:compare_rank
                in
                match Base.List.find_consecutive_duplicate l ~equal:equal_rank with
                | None -> l
                | Some (dup, _) ->
                  failwith ("Duplicate entry in diff: " ^ [%e variants "to_name"] dup)]
          in
          (* [fun _get_a _get_b -> init] *)
          List.fold_right
            var_functions
            ~f:(fun var_fn expr -> [%expr fun [%p p var_fn] -> [%e expr]])
            ~init
        in
        let struct_items =
          create
          @
          if What_to_derive.mem what_to_derive What_to_derive.Entry.sexp
             || What_to_derive.mem what_to_derive What_to_derive.Entry.of_sexp
          then [%str let t_of_sexp = [%e t_of_sexp]]
          else []
        in
        (* Since the type is private, we also expose a [of_single_list_exn] function, and we need
           to override [t_of_sexp] (if one exists) *)
        { Items.sig_items; struct_items = Ok struct_items }
      in
      ( diff_type_declaration
      , diff_type_flags
      , single_module
      , compare_functions
      , create_functions )
  in
  (* type ('a, 'b) derived_on = ('a, 'b) [type_to_diff.name] *)
  let derived_on_type_declaration =
    { Type_declaration.params
    ; name = Type_name.derived_on
    ; kind =
        (let pointer = Type_declaration.pointer type_to_diff_declaration in
         match type_to_diff_declaration.kind with
         | Core _ | Abstract -> Core pointer
         | Record { fields; local; equal_to = _ } ->
           Record { fields; local; equal_to = Some pointer }
         | Variant { rows; equal_to = _ } -> Variant { rows; equal_to = Some pointer })
    ; unboxed = type_to_diff_declaration.unboxed
    }
  in
  let fun_items =
    Functions.to_items
      functions
      ~derived_on_type_declaration
      ~diff_type_declaration
      ~builder
      ~derived_on_type_is_local:(Type_kind.is_local type_to_diff_declaration.kind)
  in
  let maybe_private_items =
    let items =
      [ Type_declaration.to_items diff_type_declaration ~context ~flags:diff_type_flags
      ; compare_functions
      ; fun_items
      ; create_functions
      ]
      |> Items.concat
    in
    if not diff_type_flags.private_
    then items
    else (
      let { Items.sig_items; struct_items } = items in
      let struct_items =
        Result.map struct_items ~f:(fun struct_items ->
          (* Add a signature in the [ml] as well in the private case. Otherwise, users would
             be able to create illegal diffs in the ml. *)
          let structure = pmod_structure struct_items in
          let module_expr =
            pmod_constraint
              structure
              ([%sig: [@@@ocaml.warning "-32"]] @ sig_items |> pmty_signature)
          in
          [%str include [%m module_expr]])
      in
      { Items.sig_items; struct_items })
  in
  (* If diff is not exposed in the mli, the [derived_on] type might be unused *)
  let ignore_unused_type_warning =
    { Items.sig_items = []; struct_items = Ok [%str [@@@ocaml.warning "-34"]] }
  in
  [ prefix
  ; ignore_unused_type_warning
  ; Type_declaration.to_items
      derived_on_type_declaration
      ~context:{ context with what_to_derive = What_to_derive.empty }
  ; additional_type
  ; maybe_private_items
  ]
  |> Items.concat
;;

let add_prefix t ~prefix = { t with prefix = Items.concat [ prefix; t.prefix ] }

let to_module t ~context ~(type_to_diff_declaration : unit Type_declaration.t) =
  let { Context.builder; _ } = context in
  to_items t ~type_to_diff_declaration ~context
  |> Items.to_module
       ~module_name:
         (Module_name.diff_module_name ~type_to_diff_name:type_to_diff_declaration.name)
       ~builder
;;
