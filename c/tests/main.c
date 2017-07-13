#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <setjmp.h>
#include <cmocka.h>

#include "test_prim_bool.h"



int main(int argc, char **argv) {

  const struct CMUnitTest tests[] = {
    cmocka_unit_test(test_prim_bool)
  };

  return cmocka_run_group_tests(tests, NULL, NULL);
}
