#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct pangram_context {
 int cnt;
 unsigned char memo[26+1];
} pangram_ctx_t;

int is_pangram(const char *);
static int _is_pangram(pangram_ctx_t *, const char *);
static int set_char(pangram_ctx_t *, char);
static int get_cnt(pangram_ctx_t *);
static void incr(pangram_ctx_t *);

int is_pangram(const char *s) {
 pangram_ctx_t pangram_ctx;

 memset(&pangram_ctx, 0, sizeof(pangram_ctx_t));
 return _is_pangram(&pangram_ctx, s);
}

int _is_pangram(pangram_ctx_t *ctx, const char *s) {
 int i;
 char c;
 if(!s || !ctx) {
  return -1;
 }
 if(*s == '\0') {
  return 0;
 }
 i = set_char(ctx, *s);
 if(get_cnt(ctx) == 26) {
  return 1;
 }
 return _is_pangram(ctx, ++s);
}

int set_char(pangram_ctx_t *ctx, char c) {
 int i;
 if(!isalpha(c)) {
  return -1;
 }
 c = tolower(c);
 i = c % 26;
 if(ctx->memo[i] == 0) {
  ctx->memo[i]++;
  incr(ctx);
 }
 return 0;
}

void incr(pangram_ctx_t *ctx) {
 ctx->cnt++;
 return;
}

int get_cnt(pangram_ctx_t *ctx) {
 return ctx->cnt;
}

/*
int _pow(int x, int y) {
 int _x = x;
 while(y > 0) {
  x = x * _x;
  y--;
 }
}
*/

int main(void) {
 static char buf[(10*10*10)+1];
 fgets(buf,sizeof(buf)-1,stdin);
 if(is_pangram(buf) == 1) {
  puts("pangram");
 } else {
  puts("not pangram");
 }
 return 0;
}
