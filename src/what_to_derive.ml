open Base
open! Ppxlib

module Entry = struct
  include String

  let sexp_of = "sexp_of"
  let of_sexp = "of_sexp"
  let sexp = "sexp"
  let bin_io = "bin_io"
  let variants = "variants"
  let equal_ = "equal"
  let compare_ = "compare"
end

module Entry_maybe_localize : sig
  type t

  val create : base_name:string -> t
  val equal_localize : t
  val compare_localize : t
  val base_name : t -> string
  val to_deriving_expression : t -> builder:Builder.t -> expression
end = struct
  type t =
    | Simple of Entry.t
    | Equal_localize
    | Compare_localize

  let create ~base_name = Simple base_name

  let base_name = function
    | Simple x -> x
    | Equal_localize -> Entry.equal_
    | Compare_localize -> Entry.compare_
  ;;

  let localize = function
    | Simple _ -> false
    | Equal_localize | Compare_localize -> true
  ;;

  let equal_localize = Equal_localize
  let compare_localize = Compare_localize

  let to_deriving_expression t ~builder =
    let open (val builder : Builder.S) in
    let localize_entry base_name =
      pexp_apply
        (pexp_ident (Located.mk (Lident base_name)))
        [ Labelled "localize", pexp_ident (Located.mk (Lident "localize")) ]
    in
    let base_name = base_name t in
    if localize t then localize_entry base_name else Build_helper.Text base_name |> e
  ;;
end

let attribute_name = "deriving"

type t = Entry_maybe_localize.t list

let empty = []

let mem (t : t) (entry : Entry.t) =
  List.exists t ~f:(fun existing ->
    String.( = ) (Entry_maybe_localize.base_name existing) entry)
;;

let add (t : t) (entry : Entry.t) =
  if mem t entry then t else t @ [ Entry_maybe_localize.create ~base_name:entry ]
;;

module Extra = struct
  type t = Entry.t list

  let label = "extra_derive"
  let pattern = Ast_pattern.(elist (pexp_ident (lident __)))
  let arg = Deriving.Args.arg label pattern
end

let create ?extra (td : type_declaration) how_to_diff sig_or_struct ~builder =
  let open (val builder : Builder.S) in
  let extra =
    match extra with
    | None -> []
    | Some [] -> raise_error (Extra.label ^ " should not be empty")
    | Some (_ :: _ as extra) -> extra
  in
  let deriving =
    List.concat_map td.ptype_attributes ~f:(fun { attr_name; attr_payload; _ } ->
      if not
           (String.( = ) attr_name.txt attribute_name
            || String.( = ) attr_name.txt (attribute_name ^ "_inline"))
      then []
      else (
        match attr_payload with
        | PSig _ | PTyp _ | PPat _ -> []
        | PStr str ->
          List.concat_map str ~f:(fun item ->
            match item.pstr_desc with
            | Pstr_eval (expr, []) ->
              let rec get (expr : expression) =
                match
                  Ppxlib_jane.Shim.Expression_desc.of_parsetree
                    expr.pexp_desc
                    ~loc:expr.pexp_loc
                with
                | Pexp_ident { txt = Lident d; _ } -> [ d ]
                | Pexp_tuple labeled_exps ->
                  (match Ppxlib_jane.as_unlabeled_tuple labeled_exps with
                   | Some exps -> List.concat_map exps ~f:get
                   | None -> [])
                | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident d; _ }; _ }, _) ->
                  [ d ]
                | _ -> []
              in
              get expr
            | _ -> [])))
  in
  let default =
    (List.filter
       deriving
       ~f:(Set.mem Entry.(Set.of_list (module Entry) [ sexp_of; of_sexp; sexp; bin_io ]))
     |> List.map ~f:(fun base_name -> Entry_maybe_localize.create ~base_name))
    @
    match (how_to_diff : How_to_diff.Atomic.t option), sig_or_struct with
    | None, _ | _, `sig_ -> []
    | Some atomic, `struct_ ->
      [ (match atomic with
         | Using_equal | Using_equal_via_get ->
           Entry_maybe_localize.create ~base_name:Entry.equal_
         | Using_equal_local -> Entry_maybe_localize.equal_localize
         | Using_compare -> Entry_maybe_localize.create ~base_name:Entry.compare_
         | Using_compare_local -> Entry_maybe_localize.compare_localize)
      ]
  in
  match List.find_all_dups extra ~compare:String.compare with
  | [] ->
    List.fold extra ~init:default ~f:(fun t entry ->
      if mem t entry
      then
        raise_error
          ("Unnecessary entry "
           ^ entry
           ^ " in "
           ^ Extra.label
           ^ ". "
           ^ entry
           ^ " is already derived by default")
      else add t entry)
  | dups ->
    raise_error
      ("Duplicate entries in " ^ Extra.label ^ ": " ^ String.concat ~sep:", " dups)
;;

let attribute t ~builder =
  let open (val builder : Builder.S) in
  let what_to_derive =
    match List.map t ~f:(Entry_maybe_localize.to_deriving_expression ~builder) with
    | [] -> None
    | [ x ] -> Some x
    | xs -> Some (pexp_tuple xs)
  in
  Option.map what_to_derive ~f:(fun what_to_derive ->
    attribute
      ~name:(Located.mk attribute_name)
      ~payload:(PStr [ pstr_eval what_to_derive [] ]))
;;
