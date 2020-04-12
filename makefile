all:
	cd anagram-generator-ada && make harness && cd ..
	cd babylonian-square-root-cobol && make && cd ..
	cd lucifer-cipher-fortran && make && cd ..
	cd russian-peasant-multiplication && make c && cd ..
	cd russian-peasant-multiplication && make ada && cd ..
	cd russian-peasant-multiplication && make fortran && cd ..
