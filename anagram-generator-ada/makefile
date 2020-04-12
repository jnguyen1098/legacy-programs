GFLAGS = -Wall -Wuninitialized -Wunused -gnatwa -gnatw.a -gnatwb -gnatw.b -gnatwc -gnatw.c -gnatwae

harness: src/scramble_with_harness.adb
	gnatmake $(GFLAGS) src/scramble_with_harness.adb -o bin/scramble
	cd bin && ./scramble && cd ..
	make clean

main: src/scramble.adb
	gnatmake $(GFLAGS) src/scramble.adb -o bin/scramble
	cd bin && ./scramble && cd ..
	make clean

clean:
	rm -rf *.ali *.o bin/scramble
