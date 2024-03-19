(* Represents the contents of a module which implements Diffable.Diff.S_plain *)
open! Base
open Ppxlib

module Functions : sig
  type t =
    { get : expression
    ; apply_exn : expression
    ; of_list_exn : expression
    }
end

module Diff_type_kind : sig
  type t =
    | This of
        { kind : unit Type_kind.t
        ; nonrec_ : bool
        ; unboxed_override : (* Default is [Type_kind.can_be_unboxed] *)
            bool option
        }
    | (* Separated out so that we can make the type private and guarantee ordering

         Example:
         {[
           type derived_on = { foo : int; bar : string }
           type single =
             | Foo of Diff_of_int.t
             | Bar of Diff_of_string.t

           type t = private single list
         ]} *)
      Sorted_list of
        { single_kind : (Variant_row_name.t * unit Type_kind.core) list
        ; single_module_name : Module_name.t
        }
end

type t =
  { prefix : Items.t
  ; diff_type : Diff_type_kind.t
  ; functions : Functions.t Or_error.t
  }

val add_prefix : t -> prefix:Items.t -> t

val to_module
  :  t
  -> context:Context.t
  -> type_to_diff_declaration:unit Type_declaration.t
  -> Items.t
