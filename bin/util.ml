type node_id = Node of int

let int_of_node (Node i) = i

type 'a identified = { node: 'a; id: node_id }
;;

let todo_failure str = Failure ("TODO:"^str)
let type_err_failure str = Failure ("type_err:"^str)

let safe_float_of_string str =
  try Some (float_of_string str)
  with Failure _ -> raise (Failure ("Err: wrong float literal: " ^ str))