#ifndef PERMUTATIONS_H
#define PERMUTATIONS_H

typedef struct btree_node {
 char *v;
 struct btree_node *lb;
 struct btree_node *rb;
} btree_node_t;

typedef struct btree {
 int sz;
 btree_node_t *root;
} btree_t;

int odd(int);
int even(int);
int swap(char *, int, int);
int nfac(int);
btree_t * heaps(btree_t *, int, char *);

btree_t * tree_init(void);
btree_node_t * tree_node_init(char *);
btree_node_t * tree_ins(btree_t *, char *);
btree_node_t * _tree_ins(btree_t *, btree_node_t *, char *);
void tree_print_inorder(btree_t *);
void _tree_print_inorder(btree_t *, btree_node_t *);

#endif
