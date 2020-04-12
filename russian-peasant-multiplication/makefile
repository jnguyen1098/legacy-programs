GNATFLAGS = -Wall -Wuninitialized -Wunused -gnatwa -gnatw.a -gnatwb -gnatw.b -gnatwc -gnatw.c -gnatwae

c: src/peasant.c
	gcc -Wall -Wextra -Werror src/peasant.c -o bin/peasant
	make run
	make clean

ada: src/peasant.adb
	gnatmake $(GNATFLAGS) src/peasant.adb -o bin/peasant
	make run
	make clean
	rm -rf peasant.ali peasant.o

fortran: src/peasant.f95
	gfortran -Wall -fcheck=all src/peasant.f95 -o bin/peasant
	make run
	make clean

run:
	./bin/peasant

clean:
	rm -rf bin/peasant
