#include <stdio.h>
#include "permutations.h"

/*
procedure generate(n : integer, A : aay of any):
    if n = 1 then
          output(A)
    else
        for i := 1; i ≤ n; i += 1 do
            generate(n - 1, A)
            if n is odd then
                j ← 1
            else
                j ← i
            swap(A[j], A[n])
*/

btree_t * tree_init(void) {
	btree_t *bt;
	bt = (btree_t *) calloc(1, sizeof(btree_t));
	return bt;
}

btree_node_t * tree_node_init(char *s) {
	btree_node_t *btn;
	btn = (btree_node_t *) calloc(1, sizeof(btree_node_t));
	btn->v = s;
	return btn;
}

btree_node_t * tree_ins(btree_t *tree, char *s) {
	if (!tree->root) {
		tree->sz++;
		tree->root = tree_node_init(s);
		return tree->root;
	} else {
		return _tree_ins(tree, tree->root, s);
	}
}

btree_node_t * _tree_ins(btree_t *tree, btree_node_t *node, char *s) {
	int cmp;

	cmp = strcmp(s, node->v);
	if (cmp == 0) {
		return node;
	} else if (cmp < 0) {
		if (node->lb == NULL) {
			tree->sz++;
			node->lb = tree_node_init(s);
			return node->lb;
		} else {
			return _tree_ins(tree, node->lb, s);	
		}
	} else if (cmp > 0) {
		if (node->rb == NULL) {
			tree->sz++;
			node->rb = tree_node_init(s);
			return node->rb;
		} else {
			return _tree_ins(tree, node->rb, s);
		}
	}

}

void tree_print_inorder(btree_t *tree) {
	if (!tree->root) {
		puts("Empty");
	} else {
		_tree_print_inorder(tree, tree->root);
	}
	return;
}

void _tree_print_inorder(btree_t *tree, btree_node_t *node) {

	if(!node) {
		return;
	}

	_tree_print_inorder(tree, node->lb);
	printf("v=%s\n", node->v);
	_tree_print_inorder(tree, node->rb);
	return;
}


int nfac(int n) {
	if(n == 0) {
		return 1;
	} else {
		return n * nfac(n-1);
	}
}

int odd(int n) { return (n % 2 != 0); }
int even(int n) { return (n % 2 == 0); }

int swap(char a[], int i, int j) {
	char p;
	p = a[j]; a[j] = a[i]; a[i] = p;
}

btree_t * heaps(btree_t *tree, int n, char a[]) {
	int i, j;
	if (n == 0) {
		tree_ins(tree, strdup(a));
	 } else {
		for (i = 0; i < n; i++) {
			heaps(tree, n-1, a);
			if (odd(n) == 1) {
				j = 0;
			} else {
				j = i;
			}
			swap(a, j, (n-1));
		}
	}
	return tree;
}
