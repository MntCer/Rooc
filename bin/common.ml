(**
  Some low-level concepts in our compiler.
    *)

(* type node_id = Node of int
let int_of_node (Node i) = i

type 'a identified = { node: 'a; id: node_id }

exception Semant_err of ((node_id option) * string)

let err (idopt:node_id option) =
  let k str =
    raise (Semant_err (idopt, str))
  in
    Printf.ksprintf k *)