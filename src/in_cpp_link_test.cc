#include "sqbe.h"

int main() {
  sq_init(SQ_TARGET_DEFAULT, stdout, "");
  sq_shutdown();
}
