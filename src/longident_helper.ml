open! Base
open Ppxlib

type 'a t =
  | Simple of 'a Nonempty_list.t
  | Functor_application of 'a t * 'a t * 'a list

let rec map t ~f =
  match t with
  | Simple xs -> Simple (Nonempty_list.map xs ~f)
  | Functor_application (functor_, arg, rest) ->
    Functor_application (map functor_ ~f, map arg ~f, List.map rest ~f)
;;

let of_longident longident ~builder =
  let open (val builder : Builder.S) in
  let rec parse longident acc =
    match longident with
    | Lident s -> Simple (s, acc)
    | Ldot (x, s) -> parse x (s :: acc)
    | Lapply (x, y) -> Functor_application (parse x [], parse y [], acc)
  in
  parse longident []
;;

let rec to_longident = function
  | Simple (hd, tl) -> List.fold tl ~init:(Lident hd) ~f:(fun acc s -> Ldot (acc, s))
  | Functor_application (functor_, arg, rest) ->
    List.fold
      rest
      ~init:(Lapply (to_longident functor_, to_longident arg))
      ~f:(fun acc s -> Ldot (acc, s))
;;

let to_expression l ~builder =
  let open (val builder : Builder.S) in
  l |> to_longident |> Located.mk |> pexp_ident
;;

let of_simple_list = function
  | [] -> None
  | hd :: tl -> Some (Simple (hd, tl))
;;

let to_simple_list t_opt ~on_functor_application ~builder =
  match t_opt with
  | None -> []
  | Some (Simple (hd, tl)) -> hd :: tl
  | Some (Functor_application (functor_, arg, _)) ->
    let open (val builder : Builder.S) in
    raise_error (Error.to_string_hum (on_functor_application (functor_, arg)))
;;

let add_suffix t_opt ~suffix =
  match t_opt with
  | None -> Simple suffix
  | Some (Simple l) -> Simple (Nonempty_list.append l (Nonempty_list.to_list suffix))
  | Some (Functor_application (functor_, arg, l)) ->
    Functor_application (functor_, arg, l @ Nonempty_list.to_list suffix)
;;
