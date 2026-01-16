// OUT: first
// OUT: second

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

  SqItemCtx str0 = sq_data_start(sq_linkage_default, "str0");
  sq_data_string("first");
  SqSymbol sym_str0 = sq_data_end();

  SqItemCtx str1 = sq_data_start(sq_linkage_default, "str1");
  sq_data_string("second");
  SqSymbol sym_str1 = sq_data_end();

  SqItemCtx str2 = sq_data_start(sq_linkage_default, "str2");
  sq_data_string("third");
  SqSymbol sym_str2 = sq_data_end();

  SqItemCtx str3 = sq_data_start(sq_linkage_default, "str3");
  sq_data_string("fourth");
  SqSymbol sym_str3 = sq_data_end();

  SqItemCtx ctx = sq_func_start(sq_linkage_export, sq_type_word, "main");

  SqRef data = sq_i_alloc8(sq_const_int(32));
  sq_i_storel(sq_ref_for_symbol(sym_str0), data);
  sq_i_storel(sq_ref_for_symbol(sym_str1), sq_i_add(sq_type_long, data, sq_const_int(8)));
  sq_i_storel(sq_ref_for_symbol(sym_str2), sq_i_add(sq_type_long, data, sq_const_int(16)));
  sq_i_storel(sq_ref_for_symbol(sym_str3), sq_i_add(sq_type_long, data, sq_const_int(24)));

  SqRef target = sq_i_alloc8(sq_const_int(16));

  sq_i_blit(data, target, 16);

  sq_i_storel(sq_const_int(0), data);
  sq_i_storel(sq_const_int(0), sq_i_add(sq_type_long, data, sq_const_int(8)));
  sq_i_storel(sq_const_int(0), sq_i_add(sq_type_long, data, sq_const_int(16)));
  sq_i_storel(sq_const_int(0), sq_i_add(sq_type_long, data, sq_const_int(24)));

  sq_i_call1(sq_type_word, sq_ref_extern("puts"), (SqCallArg){sq_type_long, target});
  sq_i_call1(sq_type_word, sq_ref_extern("puts"),
             (SqCallArg){sq_type_long, sq_i_add(sq_type_long, target, sq_const_int(8))});

  sq_i_ret(sq_const_int(0));

  sq_func_end();

  if (!sq_shutdown()) {
    return 1;
  }
  return 0;
}
