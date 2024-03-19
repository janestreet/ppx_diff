open Base
open Printf

let create ?(inlined = false) tuple ~builder ~create_core =
  let max, pos = Diffable.Tuples.max_supported in
  let open (val builder : Builder.S) in
  let n = List.length tuple in
  if n > max
  then
    raise_error
      (sprintf
         "tuples of size > %i are not supported; to increase this limit, edit %s:%i"
         max
         pos.pos_fname
         pos.pos_lnum);
  Diff_constr.create
    { params = tuple
    ; module_ =
        List.map
          ~f:Module_name.of_string
          ([ sprintf "Tuple%i" n ] @ if inlined then [ "For_inlined_tuple" ] else [])
        |> Longident_helper.of_simple_list
    ; type_name = Type_name.t
    }
    ~create_core
    ~builder
;;
