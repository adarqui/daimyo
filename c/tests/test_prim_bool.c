#include <stdio.h>
#include <setjmp.h>
#include <cmocka.h>

#include "prim/bool.h"



void test_prim_bool(void **state) {
  (void) state;

  assert_int_equal(prim_true(), 1);
  assert_int_equal(prim_false(), 0);

  return;
}
