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
	pushq	%r12
	.cfi_def_cfa_offset 16
	.cfi_offset 12, -16
	pushq	%rbp
	.cfi_def_cfa_offset 24
	.cfi_offset 6, -24
	xorl	%eax, %eax
	pushq	%rbx
	.cfi_def_cfa_offset 32
	.cfi_offset 3, -32
	call	readInt@PLT
	leaq	.LC0(%rip), %rdi
	movl	%eax, %r12d
	call	printString@PLT
	xorl	%eax, %eax
	call	readString@PLT
	movq	%rax, %rbx
	xorl	%eax, %eax
	call	readString@PLT
	movl	%r12d, %edi
	movq	%rax, %rbp
	call	printInt@PLT
	movq	%rbp, %rsi
	movq	%rbx, %rdi
	call	__concat__@PLT
	leaq	.LC1(%rip), %rdi
	movq	%rax, %rsi
	call	__concat__@PLT
	movq	%rax, %rdi
	call	printString@PLT
	popq	%rbx
	.cfi_def_cfa_offset 24
	xorl	%eax, %eax
	popq	%rbp
	.cfi_def_cfa_offset 16
	popq	%r12
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE11:
	.size	main, .-main
	.ident	"GCC: (GNU) 7.2.1 20171224"
	.section	.note.GNU-stack,"",@progbits
