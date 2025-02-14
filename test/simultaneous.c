// OUT: before
// OUT: in helper!

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

  // Start main function.
  SqItemCtx ctx = sq_func_start(sq_linkage_export, sq_type_word, "main");

  // Switch to the data context where str_1 is defined.
  SqItemCtx data1 = sq_data_start(sq_linkage_default, "str_1");
  sq_data_string("before");
  SqSymbol sym_before = sq_data_end();

  // Switch back to the main context, and call puts with str_1.
  sq_itemctx_activate(ctx);
  sq_i_call1(sq_type_word, sq_ref_extern("puts"),
             (SqCallArg){sq_type_long, sq_ref_for_symbol(sym_before)});

  // Make another function (with the first still active).
  SqItemCtx subctx = sq_func_start(sq_linkage_default, sq_type_void, "helper");

  // Then, have it suspend and make another data string.
  SqItemCtx data2 = sq_data_start(sq_linkage_default, "str_2");
  sq_data_string("in helper!");
  SqSymbol sym_data2 = sq_data_end();

  // Switch back to the helper function, and print str_2.
  sq_itemctx_activate(subctx);
  sq_i_call1(sq_type_word, sq_ref_extern("puts"),
             (SqCallArg){sq_type_long, sq_ref_for_symbol(sym_data2)});
  sq_i_ret_void();
  SqSymbol sym_helper = sq_func_end();

  // Finally, switch back to main, and emit a call to the helper function.
  sq_itemctx_activate(ctx);
  sq_i_call0(sq_type_void, sq_ref_for_symbol(sym_helper));
  sq_i_ret(sq_const_int(0));
  sq_func_end();

  if (!sq_shutdown()) {
    return 1;
  }
  return 0;
}
