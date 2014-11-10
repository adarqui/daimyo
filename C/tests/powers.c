#include <assert.h>
#include <stdarg.h>
#include <stddef.h>
#include <setjmp.h>
#include <cmocka.h>
#include "powers.h"

static void test_ceil(void **state) {
 assert_int_equal(ceilPowerOfTwo(4), 4);
 assert_int_equal(ceilPowerOfTwo(6), 8);
}

static void test_floor(void **state) {
 assert_int_equal(floorPowerOfTwo(7), 4);
}

static void test_power(void **state) {
 assert_int_equal(isPowerOfTwo_DecAndCmp(268435456), 1);
 assert_int_equal(isPowerOfTwo_DecAndCmp(268435451), 0);
 assert_int_equal(isPowerOfTwo_ComplementAndCompare(268435456), 1);
 assert_int_equal(isPowerOfTwo_ComplementAndCompare(268435451), 0);
 assert_int_equal(isPowerOfTwo_ShiftRight(268435456), 1);
 assert_int_equal(isPowerOfTwo_ShiftRight(268435451), 0);
}

int main(void) {
 const UnitTest tests[] =
 {
  unit_test(test_ceil),
  unit_test(test_floor),
  unit_test(test_power)
 };
 return run_tests(tests);
}
