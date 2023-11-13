open Ast

type roc_s_block =

type roc_s_function_param


type roc_s_function = {
  rsf_name : string;
  rsfp_self: bool;
  rsf_params : roc_s_function_param list;
  rsf_return_type : roc_type;
  rsf_body : roc_s_block;
}


type roc_s_module = {

}