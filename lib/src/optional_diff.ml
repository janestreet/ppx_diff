module Diff = struct
  type 'a t = { diff : 'a } [@@unboxed]
end

type 'a t = 'a Diff.t option

let none = None
let[@inline] return diff = Some { Diff.diff }

let[@inline] map t ~f =
  match t with
  | Some { Diff.diff } -> Some { Diff.diff = (f [@inlined hint]) diff }
  | None -> None
;;

let[@inline] bind t ~f =
  match t with
  | Some { Diff.diff } -> (f [@inlined hint]) diff
  | None -> None
;;

let both = `both_would_allocate__use_bind_instead
let[@inline] ( >>| ) x f = map x ~f
let[@inline] ( >>= ) x f = bind x ~f

module Optional_syntax = struct
  module Optional_syntax = struct
    let[@inline] is_none t =
      match t with
      | None -> true
      | Some _ -> false
    ;;

    let[@inline] unsafe_value t =
      match t with
      | Some { Diff.diff } -> diff
      | None -> failwith "[Optional_diff.unsafe_value] called on [Optional_diff.none]"
    ;;
  end
end

include Optional_syntax.Optional_syntax

let[@inline] to_option t =
  match t with
  | None -> None
  | Some { Diff.diff } -> Some diff
;;

module Let_syntax = struct
  let return = return

  module Let_syntax = struct
    let return = return
    let map = map
    let bind = bind
    let both = both

    module Open_on_rhs = struct end
  end
end
