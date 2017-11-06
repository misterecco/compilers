	.text
	.intel_syntax noprefix
	.file	"foo.ll"
	.globl	main                    # -- Begin function main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# BB#0:
	push	rax
.Lcfi0:
	.cfi_def_cfa_offset 16
	mov	edi, 4
	call	printInt
	xor	eax, eax
	pop	rcx
	ret
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc
                                        # -- End function

	.section	".note.GNU-stack","",@progbits
