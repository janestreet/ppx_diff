open! Base
open Printf

let module_name ~size = sprintf "Tuple%i" size
let diff_module_name = "Diff"
let entry_diff_module_name = "Entry_diff"
let for_inlined_tuple_module_name = "For_inlined_tuple"
let nums ~size = List.init size ~f:(( + ) 1)
let var i = sprintf "'a%i" i
let diff_var i = var i ^ "_diff"
let create_arg i = sprintf "t%i" i
let gel i = sprintf "%s Gel.t" (var i)
let vars ~size = List.map (nums ~size) ~f:var
let diff_vars ~size = vars ~size @ List.map (nums ~size) ~f:diff_var

let type_ ~name ~vars ~size =
  sprintf "(%s) %s" (String.concat (vars ~size) ~sep:", ") name
;;

let t_type = type_ ~name:"t" ~vars
let derived_on_type = type_ ~name:"derived_on" ~vars
let entry_diff_type = type_ ~name:[%string "%{entry_diff_module_name}.t"] ~vars:diff_vars
let diff_type = type_ ~name:"t" ~vars:diff_vars
let diff_type_reference = type_ ~name:(sprintf "%s.t" diff_module_name) ~vars:diff_vars
let variant_name = sprintf "T%i"

let type_declaration ~size =
  let tuple = String.concat (vars ~size) ~sep:" * " in
  [%string {|type %{t_type ~size} = %{tuple} [@@deriving sexp, bin_io]|}]
;;

let diff_type_declarations ~size ~signature =
  let maybe_private = if signature then " private" else "" in
  let variants ~size =
    List.map (nums ~size) ~f:(fun i -> sprintf "| %s of %s" (variant_name i) (diff_var i))
    |> String.concat ~sep:"\n"
  in
  let maybe_open_entry_diff =
    if signature then "" else [%string {|open %{entry_diff_module_name}|}]
  in
  [%string
    {|
      type %{derived_on_type ~size} = %{t_type ~size}

      module %{entry_diff_module_name} %{if signature then ": sig" else "= struct"}
        type %{diff_type ~size} =
          %{variants ~size}
        [@@deriving variants, sexp, bin_io, quickcheck]
      end
      %{maybe_open_entry_diff}

      type %{diff_type ~size} =%{maybe_private} %{entry_diff_type ~size} list [@@deriving sexp, bin_io, quickcheck]

|}]
;;

let for_inlined_tuple_type_declaration ~size =
  [%string
    {| type %{t_type ~size} = %{List.map (nums ~size) ~f:gel |> String.concat ~sep:" * "} [@@deriving sexp, bin_io]|}]
;;

let for_inlined_tuple_diff_type_declarations ~size =
  [%string
    {|
      type %{derived_on_type ~size} = %{t_type ~size}

      type %{diff_type ~size} = %{diff_type_reference ~size} [@@deriving sexp, bin_io, quickcheck]
  |}]
;;

let tuple_mli size =
  let nums = nums ~size in
  let derived_on_type = derived_on_type ~size in
  let diff_type = diff_type ~size in
  let get_functions =
    List.map nums ~f:(fun i ->
      [%string
        {|(from: %{var i} -> to_: %{var i} -> local_ %{diff_var i} Optional_diff.t)|}])
    |> String.concat ~sep:"\n -> "
  in
  let apply_functions' =
    List.map nums ~f:(fun i -> [%string {|(%{var i} -> %{diff_var i} -> %{var i})|}])
  in
  let apply_functions = apply_functions' |> String.concat ~sep:"\n -> " in
  let of_list_functions' =
    List.map nums ~f:(fun i ->
      [%string {|(%{diff_var i} list -> local_ %{diff_var i} Optional_diff.t)|}])
  in
  let of_list_and_apply_functions =
    List.zip_exn of_list_functions' apply_functions'
    |> List.concat_map ~f:(fun (x, y) -> [ x; y ])
    |> String.concat ~sep:"\n -> "
  in
  let function_declarations ~local =
    let derived_on_type =
      if local then sprintf "local_ %s" derived_on_type else derived_on_type
    in
    [%string
      {|
        val get : %{get_functions} -> from: %{derived_on_type} -> to_: %{derived_on_type} -> local_ %{diff_type} Optional_diff.t

        val apply_exn : %{apply_functions} -> %{derived_on_type} -> %{diff_type} -> %{derived_on_type}

        val of_list_exn : %{of_list_and_apply_functions} -> %{diff_type} list -> local_ %{diff_type} Optional_diff.t
         |}]
  in
  let create_args ~optional =
    List.map nums ~f:(fun i ->
      if optional
      then [%string {|?%{create_arg i}:%{diff_var i}|}]
      else [%string {|%{create_arg i}:%{diff_var i} option|}])
    |> String.concat ~sep:" -> "
  in
  let create_of_variants_args =
    List.map nums ~f:(fun i ->
      [%string
        {|%{create_arg i}:local_ ((%{diff_var i}, %{entry_diff_type ~size}) Of_variant.t) |}])
    |> String.concat ~sep:" -> "
  in
  [%string
    {|
    module %{module_name ~size} : sig
      %{type_declaration ~size}
      module %{diff_module_name} : sig
        %{diff_type_declarations ~size ~signature:true}

        %{function_declarations ~local:false}

        val singleton : %{entry_diff_type ~size} -> %{diff_type}

        val create : %{create_args ~optional:true} -> unit -> %{diff_type}

        val create_of_variants : %{create_of_variants_args} -> %{diff_type}
      end

      module %{for_inlined_tuple_module_name} : sig
        %{for_inlined_tuple_type_declaration ~size}

        module %{diff_module_name} : sig
          %{for_inlined_tuple_diff_type_declarations ~size}
          %{function_declarations ~local:true}
        end
      end
    end
       |}]
;;

let tuple_ml size =
  let nums = nums ~size in
  let get = sprintf "get%i" in
  let apply = sprintf "apply%i_exn" in
  let of_list = sprintf "of_list%i_exn" in
  let maybe_gel s i ~gel =
    let base = sprintf "%s%i" s i in
    if not gel then base else sprintf "{Gel.g = %s}" base
  in
  let from = maybe_gel "from_" in
  let to_ = maybe_gel "to_" in
  let derived_on = maybe_gel "derived_on" in
  let t = maybe_gel "t" in
  let apply_diff n =
    [%string
      {| let %{t n ~gel:false}, diff =
           match diff with
          | %{variant_name n} d :: tl -> %{apply n} %{derived_on n ~gel:false} d, tl
          | _ -> %{derived_on n ~gel:false}, diff
         in
         |}]
  in
  let get_diff n =
    [%string
      {| let diff =
            match%optional.Optional_diff %{get n} ~from:%{from n ~gel:false} ~to_:%{to_ n ~gel:false} with
            | None -> diff
            | Some d -> %{variant_name n} d :: diff
          in
       |}]
  in
  let of_sexp_functions =
    List.map nums ~f:(sprintf "a%i_of_sexp")
    @ List.map nums ~f:(sprintf "a%i_diff_of_sexp")
    |> String.concat ~sep:" "
  in
  let create_args ~optional =
    List.map nums ~f:(fun i ->
      [%string {|%{if optional then "?" else "~"}%{create_arg i}|}])
    |> String.concat ~sep:" "
  in
  let create_function ~value option_or_optional_diff =
    let maybe_add_diff i =
      let maybe_add i =
        match option_or_optional_diff with
        | `option ->
          [%string
            {| match %{value i} with
               | None -> diff
               | Some d -> %{variant_name i} d :: diff
           |}]
        | `optional_diff ->
          [%string
            {| match%optional.Optional_diff %{value i} with
               | None -> diff
               | Some d -> %{variant_name i} d :: diff
            |}]
      in
      [%string {|let diff =
            %{maybe_add i}
          in
        |}]
    in
    [%string
      {|let diff = [] in
      %{List.rev_map nums ~f:maybe_add_diff |> String.concat ~sep:"\n"}
      diff|}]
  in
  let diff_of_list i =
    [%string
      {|
      | %{variant_name i} d :: tl ->
        let ds, tl = List.split_while tl ~f:(function
          | %{variant_name i} _ -> true
          | _ -> false)
        in
        let ds = List.map ds ~f:(function
          | %{variant_name i} x -> x
          | _ -> assert false)
        in
        (match%optional.Optional_diff %{of_list i} (d :: ds) with
         | None -> loop acc tl
         | Some d -> loop (%{variant_name i} d :: acc) tl)
         |}]
  in
  let function_implementations ~local =
    let maybe_local = if local then "local_ " else "" in
    let gel = local in
    [%string
      {|
        let get %{List.map nums ~f:get |> String.concat ~sep:" "} ~from ~to_ =
          if Base.phys_equal from to_
          then local_ Optional_diff.none
          else (
            let %{List.map nums ~f:(from ~gel) |> String.concat ~sep:", "} = from in
            let %{List.map nums ~f:(to_ ~gel) |> String.concat ~sep:", "} = to_ in
            let diff = [] in
            %{List.rev_map nums ~f:get_diff |> String.concat ~sep:""}
            match diff with
            | [] -> local_ Optional_diff.none
            | _ :: _ -> local_ Optional_diff.return diff)


        let apply_exn %{List.map nums ~f:apply |> String.concat ~sep:" "} derived_on diff =
          let %{List.map nums ~f:(derived_on ~gel) |> String.concat ~sep:", "} = derived_on in
          %{List.map nums ~f:apply_diff |> String.concat  ~sep:""}
          match diff with
          | [] -> %{maybe_local}%{List.map nums ~f:(t ~gel) |> String.concat ~sep:","}
          | _ :: _ -> %{maybe_local}failwith "BUG: non-empty diff after apply"
        |}]
  in
  let of_list_and_apply_functions =
    List.concat_map nums ~f:(fun x -> [ of_list x; sprintf "_%s" (apply x) ])
    |> String.concat ~sep:" "
  in
  let of_list_function =
    [%string
      {|
      let of_list_exn %{of_list_and_apply_functions} ts =
        match ts with
        | [] -> local_ Optional_diff.none
        | _ :: _ ->
          match List.concat ts |> List.stable_sort ~compare:compare_rank with
          | [] -> local_ Optional_diff.return []
          | _ :: _ as diff ->
            let rec loop acc = function
              | [] -> List.rev acc
               %{List.map nums ~f:diff_of_list |> String.concat ~sep:"\n"}
            in
            local_ Optional_diff.return (loop [] diff)
         |}]
  in
  let create_arg_of_variant i =
    [%string {|(%{create_arg i} %{entry_diff_module_name}.Variants.%{create_arg i})|}]
  in
  [%string
    {|
    module %{module_name ~size} = struct
      %{type_declaration ~size}
      module %{diff_module_name} = struct
        %{diff_type_declarations ~size ~signature:false}

        let compare_rank t1 t2 =
          Int.compare (%{entry_diff_module_name}.Variants.to_rank t1) (%{entry_diff_module_name}.Variants.to_rank t2)
        ;;

        let equal_rank t1 t2 =
          Int.equal (%{entry_diff_module_name}.Variants.to_rank t1) (%{entry_diff_module_name}.Variants.to_rank t2)
        ;;

        %{function_implementations ~local:false}

        %{of_list_function}

        let singleton entry_diff = [entry_diff]

        let t_of_sexp %{of_sexp_functions} sexp =
          let l = t_of_sexp %{of_sexp_functions} sexp |> List.sort ~compare:compare_rank in
          match List.find_consecutive_duplicate l ~equal:equal_rank with
          | None -> l
          | Some (dup, _) ->
           failwith ("Duplicate entry in tuple diff: " ^ %{entry_diff_module_name}.Variants.to_name dup)

        let create %{create_args ~optional:true} () =
          %{create_function ~value:create_arg `option}

        let create_of_variants %{create_args ~optional:false} =
           %{create_function ~value:create_arg_of_variant `optional_diff}
      end

      module %{for_inlined_tuple_module_name} = struct
        %{for_inlined_tuple_type_declaration ~size}

        module %{diff_module_name} = struct
          %{for_inlined_tuple_diff_type_declarations ~size}
          open %{diff_module_name}
          open %{entry_diff_module_name}
          %{function_implementations ~local:true}

          let of_list_exn = of_list_exn
        end
      end
    end
       |}]
;;

let max_supported = 6, [%here]
let l = List.range ~start:`inclusive ~stop:`inclusive 2 (fst max_supported)
let tuples_mli () = List.map l ~f:tuple_mli |> String.concat ~sep:"\n\n"
let tuples_ml () = List.map l ~f:tuple_ml |> String.concat ~sep:"\n\n"
