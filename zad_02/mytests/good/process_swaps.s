.section .data
.section .text
    .p2align 4,,15
    .globl main
    .type main, @function
processSwaps:
    pushq %RBP
    movq %RSP, %RBP
    addq $-24, %RSP
    pushq %RBX
    pushq %R12
    pushq %R13
    pushq %R14
    pushq %R15
    movq %RSI, %RBX
    movq %R9, %R12
    movq %RDI, %R13
    movq %RDX, %R14
    movq %RSI, %R15
    movq %RCX, %R11
    movq $0, -8(%RBP)
    movq %R8, %RDI
    movq %R8, %RSI
    movq %RCX, %R8
    movq %R9, %RCX
    movq %RDX, %R9
    jmp .lbl_1
.lbl_0:
    movq %RDI, %RSI
    addq $1, %RSI
    pushq %RSI
    pushq %R11
    movq %R13, %RDI
    call id
    popq %R11
    popq %RSI
    movq %RAX, %RDI
    movq %RDI, %R13
    movq %R14, %RCX
    movq %R11, %R8
    movq %RBX, %R9
    movq %RSI, -8(%RBP)
    movq %R15, %RDI
    movq %R15, %RSI
    movq %R12, %R15
    xchgq %RBX, %R12
    xchgq %R12, %R14
    jmp .lbl_1
.lbl_1:
    cmpq %R13, -8(%RBP)
    jl .t_lbl_0
    movq %R14, %RBX
    movq %R15, %R12
    movq %R11, %R13
    movq %RDI, %R14
    movq %RCX, %R15
    jmp .lbl_2
.t_lbl_0:
    movq %RSI, %R14
    movq %R8, %R15
    movq %R9, %R11
    movq -8(%RBP), %RDI
    jmp .lbl_0
.lbl_2:
    movq %RBX, %R11
    imulq $2, %R11
    movq %R12, %RBX
    addq %R11, %RBX
    movq %R13, %R12
    imulq $3, %R12
    movq %RBX, %R13
    addq %R12, %R13
    movq %R14, %RBX
    imulq $4, %RBX
    movq %R13, %R12
    addq %RBX, %R12
    movq %R15, %RBX
    imulq $5, %RBX
    movq %R12, %R13
    addq %RBX, %R13
    movq %R13, %RDI
    call id
    movq %RAX, %RBX
    movq %RBX, %RAX
    popq %R15
    popq %R14
    popq %R13
    popq %R12
    popq %RBX
    leave
    ret
id:
    pushq %RBP
    movq %RSP, %RBP
    addq $-8, %RSP
    pushq %RBX
    movq %RDI, %RAX
    popq %RBX
    leave
    ret
main:
    pushq %RBP
    movq %RSP, %RBP
    addq $-8, %RSP
    pushq %RBX
    movq $3, %RDI
    movq $1, %RSI
    movq $2, %RDX
    movq $3, %RCX
    movq $4, %R8
    movq $5, %R9
    call processSwaps
    movq %RAX, %RBX
    movq %RBX, %RDI
    call printInt
    movq $0, %RAX
    popq %RBX
    leave
    ret
