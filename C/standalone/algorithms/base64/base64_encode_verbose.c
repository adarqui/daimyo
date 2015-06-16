#include <stdio.h>
#include <unistd.h>
 
typedef unsigned long UL;

void print_c(unsigned char c[4]) {
    printf("c[0]=%i , c[1]=%i , c[2]=%i , c[3]=%i\n", c[0], c[1], c[2], c[3]);
    return;
}
 
int main(void)
{
    const char *alpha = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                "abcdefghijklmnopqrstuvwxyz"
                "0123456789+/";
    unsigned char c[4];
    UL u, x, y, z, len, w = 0;
 
    do {
        c[1] = c[2] = 0;
 
        if (!(len = read(fileno(stdin), c, 3))) break;
        print_c(c);
        x = (UL)c[0]<<16;
        y = (UL)c[1]<<8;
        z = (UL)c[2];
        printf("c[0]<<16 = %u , c[1]<<8 = %u , c[2]=%u\n", x, y, z);
        u = x | y | z;
 
        printf("u=%u, u>>18 = %u , u>>12&63 = %u , u>>6&63 = %u, u&63 = %u\n", u, u>>18, u>>12&63, u>>6 & 63, u & 63);

        putchar(alpha[u>>18]);
        putchar(alpha[u>>12 & 63]);
        putchar(len < 2 ? '=' : alpha[u>>6 & 63]);
        putchar(len < 3 ? '=' : alpha[u & 63]);
 
        putchar('\n');
    } while (len == 3);
 
    if (w) putchar('\n');
 
    return 0;
}
