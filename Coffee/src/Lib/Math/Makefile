all:
	coffee -o lib -c src/math.coffee

test: all
	coffee -o lib -c tests/math.spec.coffee
	vows --spec lib/math.spec.js
