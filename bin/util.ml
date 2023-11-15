let todo_failure str = Failure ("TODO:"^str)

let safe_float_of_string str =
  try Some (float_of_string str)
  with Failure _ -> raise (Failure ("Err: wrong float literal: " ^ str))