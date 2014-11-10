all:
	erlc math.erl
	erlc test.erl

test: all
	erl -noshell -s test start -s init stop

clean:
	rm -f *.beam
