#ifndef PATTERNS_H
#define PATTERNS_H

typedef struct pangram_context {
 int cnt;
 unsigned char memo[26+1];
} pangram_ctx_t;

int is_pangram(const char *);
static int _is_pangram(pangram_ctx_t *, const char *);
static int set_char(pangram_ctx_t *, char);
static int get_cnt(pangram_ctx_t *);
static void incr(pangram_ctx_t *);

#endif
