(**
  This module includes some tool functions for development.

  author: Yuanfei
 *)
open Common

let todo str =
  let k todo_str = failwith todo_str in
  Printf.ksprintf k "TODO: %s" str

let bug str =
  let k bug_str = failwith bug_str in
  Printf.ksprintf k "BUG: %s" str


(*
 * #TODO: outdated helper functions, will be reconstructed later
 *)

let type_err_failure str = Failure ("type_err:"^str)

let safe_float_of_string str =
  try Some (float_of_string str)
  with Failure _ -> raise (Failure ("Err: wrong float literal: " ^ str))