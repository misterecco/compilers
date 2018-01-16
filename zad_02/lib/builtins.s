	.file	"builtins.c"
	.section	.rodata.str1.1,"aMS",@progbits,1
.LC0:
	.string	"%d\n"
	.text
	.p2align 4,,15
	.globl	printInt
	.type	printInt, @function
printInt:
.LFB24:
	.cfi_startproc
	movl	%edi, %esi
	leaq	.LC0(%rip), %rdi
	xorl	%eax, %eax
	jmp	printf@PLT
	.cfi_endproc
.LFE24:
	.size	printInt, .-printInt
	.p2align 4,,15
	.globl	printString
	.type	printString, @function
printString:
.LFB25:
	.cfi_startproc
	jmp	puts@PLT
	.cfi_endproc
.LFE25:
	.size	printString, .-printString
	.section	.rodata.str1.1
.LC1:
	.string	"runtime error"
	.text
	.p2align 4,,15
	.globl	error
	.type	error, @function
error:
.LFB26:
	.cfi_startproc
	leaq	.LC1(%rip), %rdi
	subq	$8, %rsp
	.cfi_def_cfa_offset 16
	call	puts@PLT
	movl	$1, %edi
	call	exit@PLT
	.cfi_endproc
.LFE26:
	.size	error, .-error
	.section	.rodata.str1.1
.LC2:
	.string	"%d"
	.text
	.p2align 4,,15
	.globl	readInt
	.type	readInt, @function
readInt:
.LFB27:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	leaq	.LC2(%rip), %rdi
	movq	%fs:40, %rax
	movq	%rax, 8(%rsp)
	xorl	%eax, %eax
	leaq	4(%rsp), %rsi
	call	__isoc99_scanf@PLT
	movq	8(%rsp), %rdx
	xorq	%fs:40, %rdx
	movl	4(%rsp), %eax
	jne	.L9
	addq	$24, %rsp
	.cfi_remember_state
	.cfi_def_cfa_offset 8
	ret
.L9:
	.cfi_restore_state
	call	__stack_chk_fail@PLT
	.cfi_endproc
.LFE27:
	.size	readInt, .-readInt
	.p2align 4,,15
	.globl	readString
	.type	readString, @function
readString:
.LFB28:
	.cfi_startproc
	subq	$40, %rsp
	.cfi_def_cfa_offset 48
	movq	stdin(%rip), %rdx
	leaq	16(%rsp), %rsi
	leaq	8(%rsp), %rdi
	movq	$0, 8(%rsp)
	movq	%fs:40, %rax
	movq	%rax, 24(%rsp)
	xorl	%eax, %eax
	movq	$0, 16(%rsp)
	call	getline@PLT
	movq	8(%rsp), %rdx
	movq	24(%rsp), %rcx
	xorq	%fs:40, %rcx
	movb	$0, -1(%rdx,%rax)
	movq	8(%rsp), %rax
	jne	.L13
	addq	$40, %rsp
	.cfi_remember_state
	.cfi_def_cfa_offset 8
	ret
.L13:
	.cfi_restore_state
	call	__stack_chk_fail@PLT
	.cfi_endproc
.LFE28:
	.size	readString, .-readString
	.p2align 4,,15
	.globl	__concat__
	.type	__concat__, @function
__concat__:
.LFB29:
	.cfi_startproc
	pushq	%r14
	.cfi_def_cfa_offset 16
	.cfi_offset 14, -16
	pushq	%r13
	.cfi_def_cfa_offset 24
	.cfi_offset 13, -24
	movq	%rsi, %r13
	pushq	%r12
	.cfi_def_cfa_offset 32
	.cfi_offset 12, -32
	pushq	%rbp
	.cfi_def_cfa_offset 40
	.cfi_offset 6, -40
	movq	%rdi, %r14
	pushq	%rbx
	.cfi_def_cfa_offset 48
	.cfi_offset 3, -48
	call	strlen@PLT
	movq	%r13, %rdi
	movq	%rax, %rbx
	call	strlen@PLT
	leal	1(%rbx,%rax), %edi
	movq	%rax, %rbp
	movslq	%edi, %rdi
	call	malloc@PLT
	movq	%rbx, %rdx
	movq	%rax, %r12
	movq	%r14, %rsi
	movq	%rax, %rdi
	call	memcpy@PLT
	leaq	(%r12,%rbx), %rdi
	leaq	1(%rbp), %rdx
	movq	%r13, %rsi
	call	memcpy@PLT
	popq	%rbx
	.cfi_def_cfa_offset 40
	movq	%r12, %rax
	popq	%rbp
	.cfi_def_cfa_offset 32
	popq	%r12
	.cfi_def_cfa_offset 24
	popq	%r13
	.cfi_def_cfa_offset 16
	popq	%r14
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE29:
	.size	__concat__, .-__concat__
	.ident	"GCC: (GNU) 7.2.1 20171224"
	.section	.note.GNU-stack,"",@progbits
