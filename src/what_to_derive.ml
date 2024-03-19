open Base
open! Ppxlib

module Entry = struct
  include String

  let sexp_of = "sexp_of"
  let of_sexp = "of_sexp"
  let sexp = "sexp"
  let bin_io = "bin_io"
  let variants = "variants"
  let equal = "equal"
  let compare = "compare"
end

let attribute_name = "deriving"

type t = Entry.t list

let empty = []
let add t entry = if List.exists t ~f:(Entry.( = ) entry) then t else t @ [ entry ]
let mem = List.mem ~equal:String.equal

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
                match expr.pexp_desc with
                | Pexp_ident { txt = Lident d; _ } -> [ d ]
                | Pexp_tuple list -> List.concat_map list ~f:get
                | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident d; _ }; _ }, _) ->
                  [ d ]
                | _ -> []
              in
              get expr
            | _ -> [])))
  in
  let default =
    List.filter
      deriving
      ~f:(Set.mem Entry.(Set.of_list (module Entry) [ sexp_of; of_sexp; sexp; bin_io ]))
    @
    match (how_to_diff : How_to_diff.Atomic.t option), sig_or_struct with
    | None, _ | _, `sig_ -> []
    | Some { using_compare }, `struct_ ->
      [ (if using_compare then Entry.compare else Entry.equal) ]
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
  let open Build_helper in
  match t with
  | [] -> None
  | what_to_derive ->
    let what_to_derive =
      Tuple (List.map what_to_derive ~f:(fun entry -> Text (Entry.to_string entry)))
    in
    attribute
      ~name:(Located.mk attribute_name)
      ~payload:(PStr [ pstr_eval (e what_to_derive) [] ])
    |> Option.return
;;
