open! Core

type t = One [@@deriving enumerate]

let to_int = function
  | One -> 1
;;

let of_int =
  let map = List.map all ~f:(fun t -> to_int t, t) |> Int.Map.of_alist_exn in
  fun i ~builder ->
    match Map.find map i with
    | Some t -> t
    | None ->
      let open (val builder : Builder.S) in
      raise_error
        (sprintf
           "Unknown stable version %i. Known versions: %s"
           i
           (Map.keys map |> List.map ~f:Int.to_string |> String.concat ~sep:", "))
;;
