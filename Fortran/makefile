GFLAGSX = -Wall -Wextra -pedantic -fimplicit-none -fbacktrace -Wsurprising -fcheck=mem -fcheck=do -fcheck=all

GFLAGS = -Wall -Wextra -pedantic -fimplicit-none -fbacktrace -Wsurprising -fcheck=mem -cpp

task1:
	gfortran $(GFLAGS) luc.f95
	./a.out > temp_output
	cmp ciphertext_output temp_output

task2:
	gfortran $(GFLAGS) hex.f95 luchex.f95
	./a.out > temp_output
	cmp good_output temp_output

old:
	gfortran $(GFLAGS) luco.f95
	./a.out
