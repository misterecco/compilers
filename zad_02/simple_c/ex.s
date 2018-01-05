	.file	"ex.c"
	.section	.rodata
.LC0:
	.string	"User input: "
	.text
	.globl	main
	.type	main, @function
main:
.LFB0:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	movl	$0, %eax
	call	readString@PLT
	movq	%rax, -8(%rbp)
	leaq	.LC0(%rip), %rdi
	call	printString@PLT
	movq	-8(%rbp), %rax
	movq	%rax, %rdi
	call	printString@PLT
	movl	$0, %eax
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE0:
	.size	main, .-main
	.ident	"GCC: (GNU) 7.2.1 20171128"
	.section	.note.GNU-stack,"",@progbits
