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
# author: Xinyang, Yuanfei

BASE_OUTPUT_DIR = ./out

# Ensure the output directory exists
$(shell mkdir -p $(BASE_OUTPUT_DIR))

.PHONY : run
run: all
	@input=$(word 2, $(MAKECMDGOALS)); \
    base_file=$$(basename $$input .rooc); \
    dir=$$(dirname $$input); \
	target_dir=$(BASE_OUTPUT_DIR)/$$base_file; \
    mkdir -p $$target_dir; \
    llvm_file=$$target_dir/$$base_file.ll; \
    asm_file=$$target_dir/$$base_file.s; \
    exec_file=$$target_dir/$$base_file.exe; \
    ./_build/default/bin/Rooc.exe $$input > $$llvm_file && \
    llc -relocation-model=pic $$llvm_file > $$asm_file && \
    cc -o $$exec_file $$asm_file && \
	$$exec_file 