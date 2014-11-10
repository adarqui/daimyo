all:
	gcc -Wall -O3 -c src/math.c -o lib/math.o
	gcc -Wall -O3 -c src/math_test.c -o lib/math_test.o -lcunit

test:
	gcc -Wall -O3 lib/math.o lib/math_test.o src/unit.c -lcunit -o tests/unit
