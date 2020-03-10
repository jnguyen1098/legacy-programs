skeleton:
	cobc -x -free -Wall -W sqrt.cob
	./sqrt
	rm -rf sqrt

good:
	cobc -x -free -Wall -W sqrt.cob
	./sqrt > outputs/current
	rm -rf sqrt
	diff outputs/current outputs/expected
	echo ~~~~~~~~~~~~~~~~~~~~~
