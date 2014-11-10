#include "powers.h"

int isPowerOfTwo_DecAndCmp(int n) { return (n != 0 && !(n & (n - 1))); }

int isPowerOfTwo_ComplementAndCompare(int n) { return (n != 0) && ((n & (~n + 1)) == n); }

int isPowerOfTwo_ShiftRight(int n) {
 if (((n & 1) == 0) && n > 1) {
  return isPowerOfTwo_ShiftRight(n >> 1);
 } else {
  return n == 1;
 }
}

int ceilPowerOfTwo(int n) {
 int acc = (n - 1), i, powers[] = {1,2,4,8,16,32,64};

 for(i=0; i<sizeof(powers)/sizeof(int); i++) {
  acc |= (acc >> powers[i]);
 }
 return 1 + acc;
}

int floorPowerOfTwo(int n) {
 return ceilPowerOfTwo(n) >> 1;
}

static int powers[] =
  {1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,16384,32768,
  65536,131072,262144,524288,1048576,2097152,4194304,8388608,
  16777216,33554432,67108864,134217728,268435456,536870912,
  1073741824,2147483648};
