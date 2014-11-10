#include <assert.h>
#include <stdarg.h>
#include <stddef.h>
#include <setjmp.h>
#include <cmocka.h>
#include "patterns.h"

static void test_pangram(void **state) {
 assert_int_equal(is_pangram("We promptly judged antique ivory buckles for the next prize"), 1);
 assert_int_equal(is_pangram("We promptly judged antique ivory buckles for the prize"), 0);
 assert_int_equal(is_pangram(NULL), -1);
}

int main(void) {
 const UnitTest tests[] =
 {
  unit_test(test_pangram)
 };
 return run_tests(tests);
}
