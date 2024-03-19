open Base

let diff =
  let functions =
    Or_error.error_string "diff of abstract types is only supported in mlis / signatures"
  in
  { Diff.prefix = Items.empty
  ; diff_type = This { kind = Abstract; nonrec_ = false; unboxed_override = None }
  ; functions
  }
;;
