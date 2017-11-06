	.text
	.intel_syntax noprefix
	.file	"runtime.ll"
	.globl	printInt                # -- Begin function printInt
	.p2align	4, 0x90
	.type	printInt,@function
printInt:                               # @printInt
	.cfi_startproc
# BB#0:
	push	rax
.Lcfi0:
	.cfi_def_cfa_offset 16
	mov	ecx, edi
	mov	edi, dnl
	xor	eax, eax
	mov	esi, ecx
	call	printf
	pop	rax
	ret
.Lfunc_end0:
	.size	printInt, .Lfunc_end0-printInt
	.cfi_endproc
                                        # -- End function
	.globl	printDouble             # -- Begin function printDouble
	.p2align	4, 0x90
	.type	printDouble,@function
printDouble:                            # @printDouble
	.cfi_startproc
# BB#0:                                 # %entry
	push	rax
.Lcfi1:
	.cfi_def_cfa_offset 16
	mov	edi, fnl
	mov	al, 1
	call	printf
	pop	rax
	ret
.Lfunc_end1:
	.size	printDouble, .Lfunc_end1-printDouble
	.cfi_endproc
                                        # -- End function
	.globl	printString             # -- Begin function printString
	.p2align	4, 0x90
	.type	printString,@function
printString:                            # @printString
	.cfi_startproc
# BB#0:                                 # %entry
	push	rax
.Lcfi2:
	.cfi_def_cfa_offset 16
	call	puts
	pop	rax
	ret
.Lfunc_end2:
	.size	printString, .Lfunc_end2-printString
	.cfi_endproc
                                        # -- End function
	.globl	readInt                 # -- Begin function readInt
	.p2align	4, 0x90
	.type	readInt,@function
readInt:                                # @readInt
	.cfi_startproc
# BB#0:                                 # %entry
	push	rax
.Lcfi3:
	.cfi_def_cfa_offset 16
	lea	rsi, [rsp + 4]
	mov	edi, d
	xor	eax, eax
	call	scanf
	mov	eax, dword ptr [rsp + 4]
	pop	rcx
	ret
.Lfunc_end3:
	.size	readInt, .Lfunc_end3-readInt
	.cfi_endproc
                                        # -- End function
	.globl	readDouble              # -- Begin function readDouble
	.p2align	4, 0x90
	.type	readDouble,@function
readDouble:                             # @readDouble
	.cfi_startproc
# BB#0:                                 # %entry
	push	rax
.Lcfi4:
	.cfi_def_cfa_offset 16
	mov	rsi, rsp
	mov	edi, lf
	xor	eax, eax
	call	scanf
	movsd	xmm0, qword ptr [rsp]   # xmm0 = mem[0],zero
	pop	rax
	ret
.Lfunc_end4:
	.size	readDouble, .Lfunc_end4-readDouble
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
