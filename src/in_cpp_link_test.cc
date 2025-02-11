#include "sqbe.h"

int main() {
  SqConfiguration config = SQ_CONFIGURATION_DEFAULT;
  sq_init(&config);
  sq_shutdown();
}
