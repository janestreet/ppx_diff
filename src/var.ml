open Base
include String

let of_string = Fn.id
let to_string = Fn.id

let core_type t ~builder =
  let open (val builder : Builder.S) in
  ptyp_var t
;;

let diff_var t = t ^ "_diff"
