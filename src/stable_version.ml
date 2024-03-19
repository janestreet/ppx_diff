open! Base

type t = One [@@deriving enumerate]

let to_int = function
  | One -> 1
;;

let of_int =
  let map = List.map all ~f:(fun t -> to_int t, t) |> Map.of_alist_exn (module Int) in
  fun i ~builder ->
    match Map.find map i with
    | Some t -> t
    | None ->
      let open (val builder : Builder.S) in
      raise_error
        ("Unknown stable version "
         ^ Int.to_string i
         ^ ". Known versions: "
         ^ (Map.keys map |> List.map ~f:Int.to_string |> String.concat ~sep:", "))
;;
