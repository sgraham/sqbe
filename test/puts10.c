// OUT: 0
// OUT: 1
// OUT: 2
// OUT: 3
// OUT: 4
// OUT: 5
// OUT: 6
// OUT: 7
// OUT: 8
// OUT: 9

#include "sqbe.h"

int main(int argc, char** argv) {
  if (argc != 2) {
    fprintf(stderr, "no out file\n");
    return 1;
  }
  sq_init(SQ_TARGET_AMD64_WIN, fopen(argv[1], "wb"), "");

  SqBlock b_start = sq_func_start(sq_linkage_export, sq_type_word, "main");

  SqRef y = sq_i_alloc4(sq_const_int(4));
  SqRef y1 = sq_i_add(sq_type_long, y, sq_const_int(1));
  sq_i_storeb(sq_const_int(0), y1);

  SqBlock b_loop = sq_block_declare_and_start();

  SqRef n1 = sq_ref_declare();

  SqRef n = sq_i_phi(sq_type_word, b_start, sq_const_int(0), b_loop, n1);

  SqRef c = sq_i_add(sq_type_word, n, sq_const_int('0'));
  sq_i_storeb(c, y);

  SqRef r = sq_i_call1(sq_type_word, sq_extern("puts"), (SqCallArg){sq_type_long, y});

  sq_i_add_into(n1, sq_type_word, n, sq_const_int(1));
  SqRef cmp = sq_i_cslew(sq_type_word, n1, sq_const_int(9));

  SqBlock b_end = sq_block_declare();
  sq_i_jnz(cmp, b_loop, b_end);

  sq_block_start(b_end);
  sq_i_ret(sq_const_int(0));

  sq_func_end();

  sq_shutdown();
}
