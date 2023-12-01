let todo_failure str = Failure ("TODO:"^str)
let type_err_failure str = Failure ("type_err:"^str)

let safe_float_of_string str =
  try Some (float_of_string str)
  with Failure _ -> raise (Failure ("Err: wrong float literal: " ^ str))