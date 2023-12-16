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

# compile a single "rooc" file to an executable
# author: Xinyang

.PHONY: run
run: all
	@input=$(word 2, $(MAKECMDGOALS)); \
	llvm_file=$${input%.rooc}.ll; \
	asm_file=$${input%.rooc}.s; \
	exec_file=$${input%.rooc}.exe; \
	./_build/default/bin/Rooc.exe $$input > $$llvm_file; \
	llc -relocation-model=pic $$llvm_file > $$asm_file; \
	cc -o $$exec_file $$asm_file; \
	rm $$llvm_file $$asm_file
