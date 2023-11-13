	.text
	.file	"Rooc"
	.globl	main                            // -- Begin function main
	.p2align	2
	.type	main,@function
main:                                   // @main
	.cfi_startproc
// %bb.0:                               // %entry
	str	x30, [sp, #-16]!                // 8-byte Folded Spill
	.cfi_def_cfa_offset 16
	.cfi_offset w30, -16
	adrp	x0, .Lfmt
	add	x0, x0, :lo12:.Lfmt
	bl	printf
	mov	w0, wzr
	ldr	x30, [sp], #16                  // 8-byte Folded Reload
	ret
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc
                                        // -- End function
	.type	.Lfmt,@object                   // @fmt
	.section	.rodata.str1.1,"aMS",@progbits,1
.Lfmt:
	.asciz	"Hello world"
	.size	.Lfmt, 12

	.section	".note.GNU-stack","",@progbits
