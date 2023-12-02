.PHONY : all
all : ./_build/default/bin/Rooc.exe


.PHONY : test
test : all testall.sh
	./testall.sh

./_build/default/bin/Rooc.exe : bin/parser.mly bin/scanner.mll bin/*.ml
	dune build

.PHONY : clean
clean :
	dune clean
	rm -rf testall.log *.diff Rooc.opam *.ll