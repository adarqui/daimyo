#include <assert.h>
#include <stdarg.h>
#include <stddef.h>
#include <setjmp.h>
#include <cmocka.h>
#include "prime.h"

static void test_is_prime_2(void **state) {
    assert_int_equal(is_prime_2(2), 1);
    assert_int_equal(is_prime_2(4), 0);
    assert_int_equal(is_prime_2(3), 1);
    assert_int_equal(is_prime_2(29), 1);
    assert_int_equal(is_prime_2(30), 0);
}

static void test_is_prime_3(void **state) {
    assert_int_equal(is_prime_3(2), 1);
    assert_int_equal(is_prime_3(4), 0);
    assert_int_equal(is_prime_3(3), 1);
    assert_int_equal(is_prime_3(29), 1);
    assert_int_equal(is_prime_3(30), 0);
}

static void test_is_prime_4(void **state) {
    assert_int_equal(is_prime_4(2), 1);
    assert_int_equal(is_prime_4(4), 0);
    assert_int_equal(is_prime_4(3), 1);
    assert_int_equal(is_prime_4(29), 1);
    assert_int_equal(is_prime_4(30), 0);
}

int main(void) {
    const UnitTest tests[] =
    {
        unit_test(test_is_prime_2),
        unit_test(test_is_prime_3),
        unit_test(test_is_prime_4),
    };
    return run_tests(tests);
}
