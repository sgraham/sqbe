// RET: 1
// ERR: last block misses jump

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

  sq_func_start(sq_linkage_export, sq_type_word, "main");
  sq_func_end();

  if (!sq_shutdown()) {
    return 1;
  }
  return 0;
}
