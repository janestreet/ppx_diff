open Base
open Ppxlib
open Shared

let check_no_custom_how_to_diff_on_equal_to ~equal_to ~builder =
  let (_ : unit Type_kind.core option) =
    Option.map
      equal_to
      ~f:
        (Type_kind.map_core ~f:(function
          | None -> ()
          | Some how_to_diff ->
            let open (val builder : Builder.S) in
            raise_error
              (Printf.sprintf
                 {|%s will be ignored.
On types like
   type t = some_other_type = repeated type definion
custom %s attributes are only allowed on the repeated type definition
                  |}
                 (How_to_diff.Custom.to_attribute_string how_to_diff)
                 Shared.name_of_ppx)))
  in
  ()
;;

let create_default ~context ~kind_to_diff : Diff.t =
  let { Context.builder; _ } = context in
  let create_core = Diff_core.create ~context in
  let open (val builder : Builder.S) in
  match (kind_to_diff : How_to_diff.t Type_kind.t) with
  | Core core -> create_core core |> Core_diff.to_diff
  | Abstract -> Diff_abstract.diff
  | Record { fields; local; equal_to } ->
    check_no_custom_how_to_diff_on_equal_to ~equal_to ~builder;
    Diff_record.create_record ~record_fields:fields ~local ~builder ~create_core
  | Variant { rows; equal_to } ->
    check_no_custom_how_to_diff_on_equal_to ~equal_to ~builder;
    Diff_variant.create rows ~context ~create_core
;;

let generate context type_to_diff_declaration ~how_to_diff : Items.t =
  let { Context.builder; sig_or_struct; _ } = context in
  let open (val builder : Builder.S) in
  let { Type_declaration.kind = kind_to_diff; name = _; params = _; unboxed = _ } =
    type_to_diff_declaration
  in
  let diff =
    match how_to_diff with
    | None -> create_default ~context ~kind_to_diff
    | Some atomic ->
      Diff_atomic.create ~type_to_diff_declaration ~atomic ~builder ~sig_or_struct
  in
  let prefix =
    { Items.sig_items = [%sig: open! Diffable.For_ppx]
    ; struct_items = Ok [%str open! Diffable.For_ppx]
    }
  in
  let type_to_diff_declaration =
    Type_declaration.map type_to_diff_declaration ~f:(fun _ -> ())
  in
  diff |> Diff.add_prefix ~prefix |> Diff.to_module ~context ~type_to_diff_declaration
;;

let validate_rec_flag (td : How_to_diff.t Type_declaration.t) rec_flag ~builder =
  match rec_flag with
  | Recursive -> ()
  | Nonrecursive ->
    (match td.kind with
     | Type_kind.Core (Constr { type_name; module_ = None; _ }, _)
       when Type_name.( = ) type_name td.name -> ()
     | _ ->
       (* [nonrec] won't work for most types. E.g.
          {[
            type t = int [@@deriving diff]

            module M = struct
              type nonrec t = t * string [@@deriving_inline diff]

              module Diff = struct
                type derived_on = t
                type t = (t, string, Diff.t, Diff_of_string.t) Tuple2.Diff.t

                                                                 ...
              end
            end
          ]}

          will result in "type declaration is cyclic"

          But even writing

          {[ module Diff = struct
               type derived_on = t
               type nonrec t = (t, string, Diff.t, Diff_of_string.t) Tuple2.Diff.t
             end
          ]}

          would not be enough, because the [t] in [(t, string, ...)] is the wrong [t].

          We run into similar problems with e.g. [type nonrec t = t option] or
          [type nonrec t = | X | Y of t]
       *)
       let open (val builder : Builder.S) in
       raise_error
         "[nonrec] is only supported for types that look like [type (?params) \
          [type_name] = (?params) [type_name]], e.g. [type nonrec t = t] or [type nonrec \
          x = x] or [type nonrec 'a t = 'a t] or [type nonrec 'a t = ('a, int) t] etc.")
;;

let generator sig_or_struct ~f =
  Deriving.Generator.make
    Deriving.Args.(
      empty
      +> arg How_to_diff.Label.how (Ast_pattern.estring __)
      +> arg How_to_diff.Label.key How_to_diff.Type_.pattern
      +> arg How_to_diff.Label.elt How_to_diff.Type_.pattern
      +> What_to_derive.Extra.arg
      +> arg "stable_version" (Ast_pattern.eint __))
    (fun ~(loc : Location.t)
         ~path:(_ : string)
         ((rec_flag : rec_flag), (type_declarations : type_declaration list))
         how
         key
         elt
         extra_derive
         stable_version ->
      let (builder : Builder.t) =
        Builder.create
          (module struct
            include (val Ast_builder.make loc)
            include (val Ppxlib_jane.Ast_builder.make loc)
          end)
      in
      let how_to_diff = How_to_diff.Maybe_abstract.create ~how ~key ~elt ~builder in
      let open (val builder : Builder.S) in
      let td =
        match type_declarations with
        | [] | _ :: _ :: _ -> raise_error "multiple type declarations are not supported"
        | [ td ] -> td
      in
      let type_to_diff_declaration, how_to_diff =
        Type_declaration.create td ~how_to_diff ~builder
      in
      (match really_recursive rec_flag type_declarations with
       | Recursive ->
         (match how_to_diff with
          | Some (_ : How_to_diff.Atomic.t) -> ()
          | _ -> raise_error "recursive types are not supported (except for atomic diffs)")
       | Nonrecursive -> validate_rec_flag type_to_diff_declaration rec_flag ~builder);
      let what_to_derive =
        What_to_derive.create ?extra:extra_derive td how_to_diff sig_or_struct ~builder
      in
      let context =
        { Context.builder
        ; what_to_derive
        ; all_params = type_to_diff_declaration.params
        ; sig_or_struct
        ; stable_version = Option.map stable_version ~f:(Stable_version.of_int ~builder)
        }
      in
      let t = generate context type_to_diff_declaration ~how_to_diff in
      f t ~builder)
;;

module Gen_sig = struct
  let generator = generator `sig_ ~f:(fun items ~builder:_ -> items.sig_items)
end

module Gen_struct = struct
  let generator =
    generator `struct_ ~f:(fun items ~builder ->
      match items.Items.struct_items with
      | Ok items -> items
      | Error error ->
        let open (val builder : Builder.S) in
        raise_error (Error.to_string_mach error))
  ;;
end

let () =
  Deriving.add
    name_of_ppx
    ~str_type_decl:Gen_struct.generator
    ~sig_type_decl:Gen_sig.generator
  |> Deriving.ignore
;;
