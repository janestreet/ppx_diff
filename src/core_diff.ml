open! Base

type t =
  { diff_type : unit Type_kind.core_kind
  ; functions : Diff.Functions.t
  }

let diff_type t = t.diff_type

let to_diff { diff_type; functions } =
  { Diff.prefix = Items.empty
  ; diff_type =
      This { kind = Core (diff_type, ()); nonrec_ = false; unboxed_override = None }
  ; functions = Ok functions
  }
;;

module Create = struct
  type nonrec t = How_to_diff.t Type_kind.core -> t
end
