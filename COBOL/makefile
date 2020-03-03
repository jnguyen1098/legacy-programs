skeleton:
	cobc -x -free -Wall sqrt.cob
	./sqrt
	rm -rf sqrt

good:
	cobc -x -free -Wall sqrt.cob
	./sqrt > outputs/current
	rm -rf sqrt
	diff outputs/current outputs/expected
