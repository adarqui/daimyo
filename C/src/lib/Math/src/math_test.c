#include "math.h"
#include "math_test.h"

void math_test_init(CU_pSuite p) {
 CU_add_test(p, "test_fib", test_fib);
 CU_add_test(p, "test_fact", test_fact);
 CU_add_test(p, "test_times", test_times);
 CU_add_test(p, "test_double_", test_double_);
 return;
}

void test_fib(void) {
 CU_ASSERT(fib(0) == 0);
 CU_ASSERT(fib(1) == 1);
 CU_ASSERT(fib(20) == 6765);
 return;
}

void test_fact(void) {
 CU_ASSERT(fact(0) == 1);
 CU_ASSERT(fact(5) == 120);
 return;
}

void test_times(void) {
 CU_ASSERT(times(0, 5) == 0);
 CU_ASSERT(times(5, 0) == 0);
 CU_ASSERT(times(5, 10) == 50);
 return;
}

void test_double_(void) {
 CU_ASSERT(double_(5) == 10);
 return;
}
