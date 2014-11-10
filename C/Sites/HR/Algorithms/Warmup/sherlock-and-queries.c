#include <stdio.h>
#include <string.h>
#include <stdlib.h>

// these arrays start at 1
typedef struct darray {
 int *arr;
 int sz;
} darray_t;

#define MOD ((10*10*10*10*10*10*10*10*10)+7)

darray_t read_array(char *, int);
void print_array(darray_t);

darray_t read_array(char *s, int n) {
 darray_t da;
 int i = 1;
 char * p;

 da.arr = (int *) malloc(sizeof(int)*(n+2));
 da.sz = n;

 p = strtok(s, " ");

 while(i != (n+1) && p != NULL) {
  da.arr[i] = atoi(p);
  p = strtok(NULL, " ");
  ++i;
 } 
 return da; 
}


int size_array(darray_t *da) {
 return da->sz;
}


void print_array(darray_t da) {
 int i;
 for (i = 1; i <= da.sz; i++) {
  printf("%i\n", da.arr[i]);
 }
}

void print_array_oneline(darray_t da) {
 int i;
 for (i = 1; i <= da.sz; i++) {
  printf("%i", da.arr[i]);
  if (i < da.sz) {
   putchar(' ');
  }
 }
 puts("");
}

darray_t solve(int n, int m, darray_t a, darray_t b, darray_t c) {
 int i, j;
 print_array(a);
 for (i = 1; i <= m; i++) {
  for (j = 1; j <= n; j++) {
   if ((j % b.arr[i]) == 0) {
    a.arr[j] = a.arr[j] * c.arr[i];
   }
  }
 }
 for (i = 1; i < a.sz; i++) {
  a.arr[i] = a.arr[i] % MOD; 
 }
 print_array(a);
 return a;
}

/*
for i = 1 to M do
    for j = 1 to N do
        if j % B[i] == 0 then
            A[j] = A[j] * C[i]
        endif
    end do
end do
*/


int main(void) {
 char *as, *bs, *cs;
 int n, m;
 size_t nsz, msz;
 darray_t a, b, c;

 as = bs = cs = NULL;

 scanf("%i %i\n", &n, &m);
 getline(&as, &nsz, stdin);
 getline(&bs, &msz, stdin);
 getline(&cs, &msz, stdin);
 
 a = read_array(as, n);
 b = read_array(bs, m);
 c = read_array(cs, m);

/*
 print_array(a);
 puts("--");
 print_array(b);
 puts("--");
 print_array(c);
*/

puts("SOLVING..");

 a = solve(n, m, a, b, c);
 print_array_oneline(a);
 
 return 0;
}
