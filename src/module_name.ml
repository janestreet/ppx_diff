open Base
include String

let generate ~prefix ~type_name =
  if Type_name.( = ) type_name Type_name.t
  then prefix
  else prefix ^ "_of_" ^ Type_name.to_string type_name
;;

let diff_module_name ~type_to_diff_name =
  generate ~prefix:"Diff" ~type_name:type_to_diff_name
;;
