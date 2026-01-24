// OUT: in qbe helper, env: 12345678, arg0: 456, arg1: 789
// OUT: in c helper, arg0: 456, arg1: 789

#define SQBE_IMPLEMENTATION
#include "sqbe.h"

int main(int argc, char** argv) {
  if (argc != 2) {
    fprintf(stderr, "no out file\n");
    return 1;
  }
  SqConfiguration config = SQ_CONFIGURATION_DEFAULT;
  config.output = fopen(argv[1], "wb");
  sq_init(&config);

  sq_data_start(sq_linkage_default, "str0");
  sq_data_string("in qbe helper, env: %x, arg0: %d, arg1: %d\n");
  sq_data_byte(0);
  SqSymbol str0 = sq_data_end();

  sq_func_start(sq_linkage_default, sq_type_void, "qbe_helper");
  SqRef env = sq_func_param_named(sq_type_env, "env");
  SqRef arg0 = sq_func_param_named(sq_type_word, "arg0");
  SqRef arg1 = sq_func_param_named(sq_type_word, "arg1");
  sq_i_call5(sq_type_void, sq_ref_extern("printf"),
             (SqCallArg){sq_type_long, sq_ref_for_symbol(str0)}, sq_varargs_begin,
             (SqCallArg){sq_type_long, env}, (SqCallArg){sq_type_word, arg0},
             (SqCallArg){sq_type_word, arg1});
  sq_i_ret_void();
  SqSymbol helper = sq_func_end();

  sq_func_start(sq_linkage_export, sq_type_word, "main");

  sq_i_call3(sq_type_void, sq_ref_for_symbol(helper),
      (SqCallArg){sq_type_env, sq_const_int(0x12345678)},
      (SqCallArg){sq_type_long, sq_const_int(456)},
      (SqCallArg){sq_type_long, sq_const_int(789)});

  sq_i_call3(sq_type_void, sq_ref_extern("c_helper"),
      (SqCallArg){sq_type_env, sq_const_int(0x12345678)},
      (SqCallArg){sq_type_word, sq_const_int(456)},
      (SqCallArg){sq_type_word, sq_const_int(789)});

  sq_i_ret(sq_const_int(0));

  sq_func_end();

  if (!sq_shutdown()) {
    return 1;
  }
  return 0;
}
