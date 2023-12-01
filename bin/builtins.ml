open Sast
open Util

let builtin_print_int = {
  sf_name = "print_int";
  sf_self = false;
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