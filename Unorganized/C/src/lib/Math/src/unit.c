#include "math.h"
#include "math_test.h"

#include <stdio.h>

int init_suite1(void) { return 0; }
int clean_suite1(oid) { return 0; }

int main(int argc, char *argv[]) {
 CU_pSuite pSuite = NULL;
 if (CU_initialize_registry() != CUE_SUCCESS) {
  return CU_get_error();
 }
 pSuite = CU_add_suite("Daimyo Programming Exercises -> Misc -> Math in C", init_suite1, clean_suite1);
 if (pSuite == NULL) {
  CU_cleanup_registry();
  return CU_get_error();
 }

 math_test_init(pSuite);

 CU_basic_set_mode(CU_BRM_VERBOSE);
 CU_basic_run_tests();
 CU_cleanup_registry();

 return CU_get_error();
}
