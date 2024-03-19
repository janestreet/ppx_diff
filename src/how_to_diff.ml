open! Base
open Ppxlib

module Atomic = struct
  type t = { using_compare : bool } [@@deriving enumerate, equal, compare]

  let to_string { using_compare } =
    "atomic" ^ if using_compare then "_using_compare" else ""
  ;;
end

module Label = struct
  let how = "how"
  let key = "key"
  let elt = "elt"
end

module Type_ = struct
  type t = core_type

  let pattern =
    let open Ast_pattern in
    let ident =
      map' (pexp_ident __) ~f:(fun loc _ lid ->
        Some
          (Ast_builder.Default.ptyp_constr
             ~loc
             (Ast_builder.Default.Located.mk ~loc lid)
             []))
    in
    let type_ = pexp_extension (extension (string "diff.type") (ptyp __)) in
    alt ident type_
  ;;

  let optional_pattern ~label =
    let open Ast_pattern in
    let pattern =
      Ast_pattern.(
        single_expr_payload (pexp_constraint (pexp_ident (lident (string label))) __))
    in
    alt_option pattern (pstr nil)
  ;;
end

module Custom = struct
  module Flat = struct
    type t =
      | Atomic of Atomic.t
      | As_set
      | As_map
    [@@deriving enumerate, compare]

    let to_string = function
      | Atomic atomic -> Atomic.to_string atomic
      | As_set -> "set"
      | As_map -> "map"
    ;;

    let attribute_txt t =
      let name = Shared.name_of_ppx in
      "ppx_" ^ name ^ "." ^ name ^ "." ^ to_string t
    ;;

    let to_attribute_string t =
      "[@" ^ String.filter (attribute_txt t) ~f:(Char.( <> ) '@') ^ "]"
    ;;
  end

  type t =
    | Atomic of Atomic.t
    | As_set of { elt : core_type option }
    | As_map of { key : core_type option }

  let attribute (flat : Flat.t) context_kind : (_, t) Attribute.t =
    let attribute_txt = Flat.attribute_txt flat in
    let declare pattern return =
      Attribute.declare attribute_txt context_kind pattern return
    in
    match flat with
    | Atomic atomic -> declare Ast_pattern.(pstr nil) (Atomic atomic)
    | As_set ->
      declare (Type_.optional_pattern ~label:Label.elt) (fun elt -> As_set { elt })
    | As_map ->
      declare (Type_.optional_pattern ~label:Label.key) (fun key -> As_map { key })
  ;;

  let of_attributes context_kind =
    let attribute flat = attribute flat context_kind in
    let attrs = List.map Flat.all ~f:attribute in
    Staged.stage (fun value ~builder ->
      match List.filter_map attrs ~f:(fun attr -> Attribute.get attr value) with
      | [] -> None
      | [ t ] -> Some t
      | _ :: _ ->
        let open (val builder : Builder.S) in
        raise_error
          (Printf.sprintf
             "at most one of %s can be specified"
             (List.map Flat.all ~f:Flat.to_attribute_string
              |> List.sort ~compare:String.compare
              |> String.concat ~sep:", ")))
  ;;

  let to_flat = function
    | Atomic atomic -> Flat.Atomic atomic
    | As_set _ -> As_set
    | As_map _ -> As_map
  ;;

  let to_string t = Flat.to_string (to_flat t)
  let to_attribute_string t = Flat.to_attribute_string (to_flat t)
  let of_core_type = of_attributes Core_type |> Staged.unstage
  let of_label_declaration = of_attributes Label_declaration |> Staged.unstage
  let of_constructor_declaration = of_attributes Constructor_declaration |> Staged.unstage
  let of_rtag = of_attributes Rtag |> Staged.unstage

  module Or_abstract = struct
    module Flat = struct
      type t =
        | Custom of Flat.t
        | Abstract
      [@@deriving enumerate, compare]

      let to_string = function
        | Custom custom -> Flat.to_string custom
        | Abstract -> "abstract"
      ;;

      let of_string =
        let all =
          List.map all ~f:(fun t -> to_string t, t) |> Map.of_alist_exn (module String)
        in
        fun s ~builder ->
          let open (val builder : Builder.S) in
          match Map.find all s with
          | None ->
            raise_error
              (Printf.sprintf
                 "Unknown how to diff: %s. Known values are: %s"
                 s
                 (Map.keys all |> String.concat ~sep:", "))
          | Some t -> t
      ;;
    end

    type nonrec t =
      | Custom of t
      | Abstract

    let to_flat t : Flat.t =
      match t with
      | Custom custom -> Custom (to_flat custom)
      | Abstract -> Abstract
    ;;

    let to_string t = Flat.to_string (to_flat t)
  end
end

module Maybe_abstract = struct
  type t = Custom.Or_abstract.t option

  let create ~how ~key ~elt ~builder =
    let open Custom.Or_abstract in
    let open (val builder : Builder.S) in
    let how = Option.map how ~f:(Flat.of_string ~builder) in
    let error ~not_supported =
      let label = function
        | `key -> Label.key
        | `elt -> Label.elt
      in
      let supported_for = function
        | `key -> Flat.Custom As_map
        | `elt -> Flat.Custom As_set
      in
      let did_you_mean =
        match not_supported with
        | `key -> `elt
        | `elt -> `key
      in
      let maybe_hint =
        match how with
        | None -> ""
        | Some how ->
          if Flat.compare (supported_for did_you_mean) how <> 0
          then ""
          else
            Printf.sprintf
              " Since you are using %s=\"%s\", did you mean to use \"%s\" instead of \
               \"%s\"?"
              Label.how
              (Flat.to_string how)
              (label did_you_mean)
              (label not_supported)
      in
      raise_error
        (Printf.sprintf
           "\"%s\" can only be specified when %s=\"%s\".%s"
           (label not_supported)
           Label.how
           (Flat.to_string (supported_for not_supported))
           maybe_hint)
    in
    match how, key, elt with
    | None, None, None -> None
    | Some Abstract, None, None -> Some Abstract
    | Some (Custom (Atomic atomic)), None, None -> Some (Custom (Atomic atomic))
    | Some (Custom As_map), key, None -> Some (Custom (As_map { key }))
    | Some (Custom As_set), None, elt -> Some (Custom (As_set { elt }))
    | _, Some _, _ -> error ~not_supported:`key
    | _, _, Some _ -> error ~not_supported:`elt
  ;;
end

type t = Custom.t option
