(*
 * This module goes near the *bottom* of the dependency DAG, 
 * providing some basic infrastructure for the whole project.
 *)

type node_id = Node of int
let int_of_node (Node i) = i

type 'a identified = { node: 'a; id: node_id }

let bug str =
  let k bug_str = failwith bug_str in
  Printf.ksprintf k "BUG: %s" str

exception Semant_err of ((node_id option) * string)

let err (idopt:node_id option) =
  let k str =
    raise (Semant_err (idopt, str))
  in
    Printf.ksprintf k

let todo str =
  let k todo_str = failwith todo_str in
  Printf.ksprintf k "TODO: %s" str

(*
 * #TODO: outdated helper functions, will be reconstructed later
 *)

let type_err_failure str = Failure ("type_err:"^str)

let safe_float_of_string str =
  try Some (float_of_string str)
  with Failure _ -> raise (Failure ("Err: wrong float literal: " ^ str))