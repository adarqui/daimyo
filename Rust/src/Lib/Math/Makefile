all:
	rustc --crate-type=lib math.rs

test: all
	rustc --test math.rs -o math
	./math

clean:
	rm -f math
