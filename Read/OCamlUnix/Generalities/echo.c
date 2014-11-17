#include <stdio.h>

void echo(int argc, char **argv) {
    int i;
    printf("%s", argv[0]);
    for (i=1; i < argc; i++) {
        putchar(' ');
        printf("%s", argv[i]);
    }
    putchar('\n');
}

int main(int argc, char **argv) {
    if (argc > 1) {
        echo (argc-1, &argv[1]);
    }
}
