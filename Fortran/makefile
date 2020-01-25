GFLAGSX = -Wall -Wextra -pedantic -fimplicit-none -fbacktrace -Wsurprising -fcheck=mem -fcheck=do -fcheck=all

GFLAGS = -Wall -Wextra -pedantic -fimplicit-none -fbacktrace -Wsurprising -fcheck=mem -cpp

main:
	gfortran $(GFLAGS) hex.f95 luchex.f95
	./a.out
