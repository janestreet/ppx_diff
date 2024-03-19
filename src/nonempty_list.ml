open! Base

type 'a t = 'a * 'a list

let map (hd, tl) ~f = f hd, List.map ~f tl
let to_list (hd, tl) = hd :: tl
let append (hd, tl) l = hd, tl @ l

let drop_last (hd, tl) =
  match tl with
  | [] -> []
  | _ -> hd :: List.drop_last_exn tl
;;

let last (hd, tl) =
  match tl with
  | [] -> hd
  | tl -> List.last_exn tl
;;
