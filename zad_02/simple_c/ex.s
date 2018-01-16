	.file	"ex.c"
	.section	.rodata.str1.1,"aMS",@progbits,1
.LC0:
	.string	"Please enter some text"
.LC1:
	.string	"User input: "
	.section	.text.startup,"ax",@progbits
	.p2align 4,,15
	.globl	main
	.type	main, @function
main:
.LFB11:
	.cfi_startproc
	leaq	.LC0(%rip), %rdi
	subq	$8, %rsp
	.cfi_def_cfa_offset 16
	call	printString@PLT
	xorl	%eax, %eax
	call	readString@PLT
	leaq	.LC1(%rip), %rdi
	movq	%rax, %rsi
	call	__concat__@PLT
	movq	%rax, %rdi
	call	printString@PLT
	xorl	%eax, %eax
	addq	$8, %rsp
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE11:
	.size	main, .-main
	.ident	"GCC: (GNU) 7.2.1 20171224"
	.section	.note.GNU-stack,"",@progbits
