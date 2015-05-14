#include <assert.h>
#include <stdarg.h>
#include <stddef.h>
#include <setjmp.h>
#include <cmocka.h>
#include "permutations.h"

static void test_permutations(void **state) {
 btree_t *tree;
 char string[strlen("string")+1];
 char abcdef[strlen("abcdef")+1];
 char abcdefghijkl[strlen("abcdefghijkl")+1];
 char *s;

 s = &string;
 strcpy(string, "string");
 tree = tree_init();
 heaps(tree,strlen(s),s);
 assert_int_equal(tree->sz, nfac(strlen(s)));

 s = &abcdef;
 strcpy(abcdef, "abcdef");
 tree = tree_init();
 heaps(tree,strlen(s),s);
 assert_int_equal(tree->sz, nfac(strlen(s)));

 tree_print_inorder(tree);

/*
 s = &abcdefghijkl;
 strcpy(abcdefghijkl, "abcdefghijkl");
 tree = tree_init();
 heaps(tree,strlen(s),s);
 assert_int_equal(tree->sz, nfac(strlen(s)));
*/

}

int main(void) {
 const UnitTest tests[] =
 {
  unit_test(test_permutations),
 };
 return run_tests(tests);
}
