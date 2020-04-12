GFLAGS = -Wall -Wuninitialized -Wunused -gnatwa -gnatw.a -gnatwb -gnatw.b -gnatwc -gnatw.c -gnatwae

main:
	gnatmake $(GFLAGS) wordscram.adb
	./wordscram
	rm -rf wordscram.ali wordscram.o

dirty:
	gnatmake -Wall wordscram.adb
	./wordscram
	rm -rf wordscram.ali wordscram.o

clean:
	rm -rf wordscram.ali wordscram.o
