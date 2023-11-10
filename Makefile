.PHONY : all
all : ./_build/default/bin/Rooc.exe


.PHONY : test
test : all testprint.sh
	./testprint.sh

./_build/default/bin/Rooc.exe : bin/parser.mly bin/scanner.mll bin/codegen.ml bin/semant.ml bin/rooc.ml
	dune build

.PHONY : clean
clean :
	dune clean