	.file	"ex.c"
	.text
	.p2align 4,,15
	.globl	id
	.type	id, @function
id:
.LFB11:
	.cfi_startproc
	movl	%edi, %eax
	ret
	.cfi_endproc
.LFE11:
	.size	id, .-id
	.p2align 4,,15
	.globl	processSwaps
	.type	processSwaps, @function
processSwaps:
.LFB12:
	.cfi_startproc
	testl	%edi, %edi
	jle	.L6
	xorl	%eax, %eax
	jmp	.L5
	.p2align 4,,10
	.p2align 3
.L7:
	movl	%esi, %r10d
	movl	%r9d, %esi
	movl	%r8d, %r9d
	movl	%ecx, %r8d
	movl	%edx, %ecx
	movl	%r10d, %edx
.L5:
	addl	$1, %eax
	cmpl	%eax, %edi
	jne	.L7
.L4:
	leal	(%r9,%rsi,2), %eax
	leal	(%rdx,%rdx,2), %edx
	addl	%edx, %eax
	leal	(%r8,%r8,4), %edx
	leal	(%rax,%rcx,4), %eax
	addl	%edx, %eax
	ret
	.p2align 4,,10
	.p2align 3
.L6:
	movl	%r9d, %eax
	movl	%esi, %r9d
	movl	%edx, %esi
	movl	%ecx, %edx
	movl	%r8d, %ecx
	movl	%eax, %r8d
	jmp	.L4
	.cfi_endproc
.LFE12:
	.size	processSwaps, .-processSwaps
	.section	.text.startup,"ax",@progbits
	.p2align 4,,15
	.globl	main
	.type	main, @function
main:
.LFB13:
	.cfi_startproc
	subq	$8, %rsp
	.cfi_def_cfa_offset 16
	movl	$40, %edi
	call	printInt@PLT
	xorl	%eax, %eax
	addq	$8, %rsp
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE13:
	.size	main, .-main
	.ident	"GCC: (GNU) 7.2.1 20171224"
	.section	.note.GNU-stack,"",@progbits
