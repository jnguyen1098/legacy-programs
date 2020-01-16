task1:
	gfortran -Wall luc.f95
	./a.out > temp_output
	cmp good_output temp_output

task2:
	gfortran -Wall hex.f95 luchex.f95
	./a.out
	./a.out > temp_output
	cmp good_output temp_output
