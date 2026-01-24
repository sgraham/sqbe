// OUT: formatted: 123 456 789

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

  sq_data_start(sq_linkage_default, "fmt");
  sq_data_string("formatted: %d %d %d\n\0");
  SqSymbol fmt = sq_data_end();

  sq_func_start(sq_linkage_export, sq_type_word, "main");
  sq_i_call5(sq_type_word, sq_ref_extern("printf"),
             (SqCallArg){sq_type_long, sq_ref_for_symbol(fmt)}, sq_varargs_begin,
             (SqCallArg){sq_type_word, sq_const_int(123)},
             (SqCallArg){sq_type_word, sq_const_int(456)},
             (SqCallArg){sq_type_word, sq_const_int(789)});

  sq_i_ret(sq_const_int(0));

  sq_func_end();

  if (!sq_shutdown()) {
    return 1;
  }
  return 0;
}
