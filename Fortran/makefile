GFLAGSX = -Wall -Wextra -pedantic -fimplicit-none -fbacktrace -Wsurprising -fcheck=mem -fcheck=do -fcheck=all

GFLAGS = -Wall -Wextra -pedantic -fimplicit-none -fbacktrace -Wsurprising -fcheck=mem

task1:
	gfortran $(GFLAGS) luc.f95
	./a.out > temp_output
	cmp good_output temp_output

task2:
	gfortran $(GFLAGS) hex.f95 luchex.f95
	./a.out > temp_output
	cmp good_output temp_output

task1x:
	gfortran $(GFLAGSX) luc.f95
	./a.out

task2x:
	gfortran $(GFLAGSX) hex.f95 luchex.f95
	./a.out
