	.text
	.file	"llvm-link"
	.globl	main                    # -- Begin function main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# BB#0:
	pushq	%rax
.Lcfi0:
	.cfi_def_cfa_offset 16
	movl	$4, %edi
	callq	printInt
	xorl	%eax, %eax
	popq	%rcx
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc
                                        # -- End function
	.globl	printInt                # -- Begin function printInt
	.p2align	4, 0x90
	.type	printInt,@function
printInt:                               # @printInt
	.cfi_startproc
# BB#0:
	pushq	%rax
.Lcfi1:
	.cfi_def_cfa_offset 16
	movl	%edi, %ecx
	movl	$dnl, %edi
	xorl	%eax, %eax
	movl	%ecx, %esi
	callq	printf
	popq	%rax
	retq
.Lfunc_end1:
	.size	printInt, .Lfunc_end1-printInt
	.cfi_endproc
                                        # -- End function
	.globl	printDouble             # -- Begin function printDouble
	.p2align	4, 0x90
	.type	printDouble,@function
printDouble:                            # @printDouble
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Lcfi2:
	.cfi_def_cfa_offset 16
	movl	$fnl, %edi
	movb	$1, %al
	callq	printf
	popq	%rax
	retq
.Lfunc_end2:
	.size	printDouble, .Lfunc_end2-printDouble
	.cfi_endproc
                                        # -- End function
	.globl	printString             # -- Begin function printString
	.p2align	4, 0x90
	.type	printString,@function
printString:                            # @printString
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Lcfi3:
	.cfi_def_cfa_offset 16
	callq	puts
	popq	%rax
	retq
.Lfunc_end3:
	.size	printString, .Lfunc_end3-printString
	.cfi_endproc
                                        # -- End function
	.globl	readInt                 # -- Begin function readInt
	.p2align	4, 0x90
	.type	readInt,@function
readInt:                                # @readInt
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Lcfi4:
	.cfi_def_cfa_offset 16
	leaq	4(%rsp), %rsi
	movl	$d, %edi
	xorl	%eax, %eax
	callq	scanf
	movl	4(%rsp), %eax
	popq	%rcx
	retq
.Lfunc_end4:
	.size	readInt, .Lfunc_end4-readInt
	.cfi_endproc
                                        # -- End function
	.globl	readDouble              # -- Begin function readDouble
	.p2align	4, 0x90
	.type	readDouble,@function
readDouble:                             # @readDouble
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Lcfi5:
	.cfi_def_cfa_offset 16
	movq	%rsp, %rsi
	movl	$lf, %edi
	xorl	%eax, %eax
	callq	scanf
	movsd	(%rsp), %xmm0           # xmm0 = mem[0],zero
	popq	%rax
	retq
.Lfunc_end5:
	.size	readDouble, .Lfunc_end5-readDouble
	.cfi_endproc
                                        # -- End function
	.type	dnl,@object             # @dnl
	.section	.rodata,"a",@progbits
dnl:
	.asciz	"%d\n"
	.size	dnl, 4

	.type	fnl,@object             # @fnl
fnl:
	.asciz	"%.1f\n"
	.size	fnl, 6

	.type	d,@object               # @d
d:
	.asciz	"%d"
	.size	d, 3

	.type	lf,@object              # @lf
lf:
	.asciz	"%lf"
	.size	lf, 4


	.section	".note.GNU-stack","",@progbits
