(* Represents a diff of a [core] type and a way to get / apply that diff.

   Unlike [Diff], it doesn't necessarily need to go into a module, but instead can be used
   as a building block for other [Diff]s/[Core_diff]s in the same way [core] types can be
   used as building blocks for other types.
*)

open! Base

type t =
  { diff_type : unit Type_kind.core_kind
  ; functions : Diff.Functions.t
  }

val diff_type : t -> unit Type_kind.core_kind
val to_diff : t -> Diff.t

module Create : sig
  (* Note that there is only one instantiation of this type, but we factor it out as a type
     to break an otherwise complicated recursive function. Doing so allows us to split up
     our diff-generating code across multiple files. *)
  type nonrec t = How_to_diff.t Type_kind.core -> t
end
