# luc algorithm that uses module and hex
feistel_hex: src/feistel_hex.f95 src/hex_mod.f95
	gfortran -Wall -fcheck=all src/hex_mod.f95 src/feistel_hex.f95 -o bin/feistel
	make run
	make clean

# standalone refactored luc algorithm
feistel: src/feistel.f95
	gfortran -Wall -fcheck=all src/feistel.f95 -o bin/feistel
	make run
	make clean

run:
	./bin/feistel

clean:
	rm -rf bin/feistel hex.mod

