# modularized version
babylon_mod:
	cobc -x -free -Wall -d src/babylon_mod.cob -o bin/babylon_mod
	cobc -m -free -Wall -d src/sqrtmod.cob -o bin/sqrtmod.so
	cd bin && ./babylon_mod && cd ..
	make clean

# integrated version
babylon:
	cobc -x -free -Wall -d src/babylon.cob -o bin/babylon
	./bin/babylon
	make clean

clean:
	rm -rf bin/babylon_mod bin/sqrtmod.so bin/babylon
