open Sast
open Util


let print_int = {
  sf_name = "print_int";
  sf_params = Some ({
    sp_params=[{
    sv_name = "i";
    sv_type = ST_int;
    sv_initial_value = None;
  }]});
  sf_type = {
    sft_params_type = [ST_int];
    sft_return_type = ST_unit;
  };
  sf_body = BuiltIn;
}

let print_int_ir = raise (Failure "print_int_ir")



let builtins = [
  print_int;
]

let builtins_ir = [
  print_int_ir;
]