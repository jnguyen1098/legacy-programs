main:
	cobc -x -free -Wall sqrt.cob
	./sqrt > outputs/current
	rm -rf sqrt
	diff outputs/current outputs/expected
