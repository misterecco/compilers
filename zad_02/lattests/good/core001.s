.section .data
.str_3:
    .string ""
.str_2:
    .string "/* world"
.str_0:
    .string "="
.str_1:
    .string "hello */"
.section .text
    .p2align 4,,15
    .globl main
    .type main, @function
main:
    pushq %RBP
    movq %RSP, %RBP
    addq $-8, %RSP
    pushq %RBX
    pushq %R12
    pushq %R13
    pushq %R14
    pushq %R15
    movq $10, %RDI
    call fac
    movq %RAX, %RBX
    movq %RBX, %RDI
    call printInt
    movq $10, %RDI
    call rfac
    movq %RAX, %RBX
    movq %RBX, %RDI
    call printInt
    movq $10, %RDI
    call mfac
    movq %RAX, %RBX
    movq %RBX, %RDI
    call printInt
    movq $10, %RDI
    call ifac
    movq %RAX, %RBX
    movq %RBX, %RDI
    call printInt
    movq $1, %RBX
    movq $10, %R12
    movq $1, %R13
    jmp .lbl_1
.lbl_0:
    movq %RBX, %R13
    imulq %R12, %R13
    movq %R12, %RBX
    subq $1, %RBX
    movq %RBX, %R12
    movq %R13, %RBX
    jmp .lbl_1
.lbl_1:
    cmpq $0, %R12
    jg .t_lbl_0
    movq %R13, %RBX
    jmp .lbl_2
.t_lbl_0:
    jmp .lbl_0
.lbl_2:
    movq %RBX, %RDI
    call printInt
    leaq .str_0, %RDI
    movq $60, %RSI
    call repStr
    movq %RAX, %RBX
    movq %RBX, %RDI
    call printString
    leaq .str_1, %RDI
    call printString
    leaq .str_2, %RDI
    call printString
    movq $0, %RAX
    popq %R15
    popq %R14
    popq %R13
    popq %R12
    popq %RBX
    leave
    ret
fac:
    pushq %RBP
    movq %RSP, %RBP
    addq $-8, %RSP
    pushq %RBX
    pushq %R12
    pushq %R13
    pushq %R14
    pushq %R15
    movq $1, %RBX
    movq %RDI, %R12
    movq $1, %R13
    jmp .lbl_4
.lbl_3:
    movq %RBX, %R13
    imulq %R12, %R13
    movq %R12, %RBX
    subq $1, %RBX
    movq %RBX, %R12
    movq %R13, %RBX
    jmp .lbl_4
.lbl_4:
    cmpq $0, %R12
    jg .t_lbl_1
    movq %R13, %RBX
    jmp .lbl_5
.t_lbl_1:
    jmp .lbl_3
.lbl_5:
    movq %RBX, %RAX
    popq %R15
    popq %R14
    popq %R13
    popq %R12
    popq %RBX
    leave
    ret
rfac:
    pushq %RBP
    movq %RSP, %RBP
    addq $-8, %RSP
    pushq %RBX
    pushq %R12
    pushq %R13
    pushq %R14
    pushq %R15
    cmpq $0, %RDI
    je .t_lbl_2
    movq %RDI, %RBX
    jmp .lbl_7
.t_lbl_2:
    jmp .lbl_6
.lbl_6:
    movq $1, %RAX
    popq %R15
    popq %R14
    popq %R13
    popq %R12
    popq %RBX
    leave
    ret
.lbl_7:
    movq %RBX, %R12
    subq $1, %R12
    movq %R12, %RDI
    call rfac
    movq %RAX, %R13
    movq %RBX, %R12
    imulq %R13, %R12
    movq %R12, %RAX
    popq %R15
    popq %R14
    popq %R13
    popq %R12
    popq %RBX
    leave
    ret
.lbl_8:
    popq %R15
    popq %R14
    popq %R13
    popq %R12
    popq %RBX
    leave
    ret
mfac:
    pushq %RBP
    movq %RSP, %RBP
    addq $-8, %RSP
    pushq %RBX
    pushq %R12
    pushq %R13
    pushq %R14
    pushq %R15
    cmpq $0, %RDI
    je .t_lbl_3
    movq %RDI, %RBX
    jmp .lbl_10
.t_lbl_3:
    jmp .lbl_9
.lbl_9:
    movq $1, %RAX
    popq %R15
    popq %R14
    popq %R13
    popq %R12
    popq %RBX
    leave
    ret
.lbl_10:
    movq %RBX, %R12
    subq $1, %R12
    movq %R12, %RDI
    call nfac
    movq %RAX, %R13
    movq %RBX, %R12
    imulq %R13, %R12
    movq %R12, %RAX
    popq %R15
    popq %R14
    popq %R13
    popq %R12
    popq %RBX
    leave
    ret
.lbl_11:
    popq %R15
    popq %R14
    popq %R13
    popq %R12
    popq %RBX
    leave
    ret
nfac:
    pushq %RBP
    movq %RSP, %RBP
    addq $-8, %RSP
    pushq %RBX
    pushq %R12
    pushq %R13
    pushq %R14
    pushq %R15
    cmpq $0, %RDI
    jne .t_lbl_4
    jmp .lbl_13
.t_lbl_4:
    movq %RDI, %RBX
    jmp .lbl_12
.lbl_12:
    movq %RBX, %R12
    subq $1, %R12
    movq %R12, %RDI
    call mfac
    movq %RAX, %R13
    movq %R13, %R12
    imulq %RBX, %R12
    movq %R12, %RAX
    popq %R15
    popq %R14
    popq %R13
    popq %R12
    popq %RBX
    leave
    ret
.lbl_13:
    movq $1, %RAX
    popq %R15
    popq %R14
    popq %R13
    popq %R12
    popq %RBX
    leave
    ret
.lbl_14:
    popq %R15
    popq %R14
    popq %R13
    popq %R12
    popq %RBX
    leave
    ret
ifac:
    pushq %RBP
    movq %RSP, %RBP
    addq $-8, %RSP
    pushq %RBX
    pushq %R12
    pushq %R13
    pushq %R14
    pushq %R15
    movq %RDI, 0(%RSP)
    pushq %RDI
    movq $1, %RDI
    movq 0(%RSP), %RSI
    subq $8, %RSP
    call ifac2f
    addq $8, %RSP
    movq %RAX, %RBX
    movq %RBX, %RAX
    popq %R15
    popq %R14
    popq %R13
    popq %R12
    popq %RBX
    leave
    ret
ifac2f:
    pushq %RBP
    movq %RSP, %RBP
    addq $-8, %RSP
    pushq %RBX
    pushq %R12
    pushq %R13
    pushq %R14
    pushq %R15
    cmpq %RSI, %RDI
    je .t_lbl_5
    movq %RDI, %RBX
    movq %RSI, %R12
    jmp .lbl_16
.t_lbl_5:
    movq %RSI, %RBX
    movq %RDI, %R12
    jmp .lbl_15
.lbl_15:
    movq %R12, %RAX
    popq %R15
    popq %R14
    popq %R13
    popq %R12
    popq %RBX
    leave
    ret
.lbl_16:
    cmpq %R12, %RBX
    jg .t_lbl_6
    jmp .lbl_18
.t_lbl_6:
    jmp .lbl_17
.lbl_17:
    movq $1, %RAX
    popq %R15
    popq %R14
    popq %R13
    popq %R12
    popq %RBX
    leave
    ret
.lbl_18:
    movq %RBX, %R13
    addq %R12, %R13
    movq %R13, %RAX
    cqo
    movq $2, %R10
    idivq %R10
    movq %RAX, %R14
    movq %RBX, %RDI
    movq %R14, %RSI
    call ifac2f
    movq %RAX, %R13
    movq %R14, %RBX
    addq $1, %RBX
    movq %RBX, %RDI
    movq %R12, %RSI
    call ifac2f
    movq %RAX, %R14
    movq %R13, %RBX
    imulq %R14, %RBX
    movq %RBX, %RAX
    popq %R15
    popq %R14
    popq %R13
    popq %R12
    popq %RBX
    leave
    ret
repStr:
    pushq %RBP
    movq %RSP, %RBP
    addq $-8, %RSP
    pushq %RBX
    pushq %R12
    pushq %R13
    pushq %R14
    pushq %R15
    leaq .str_3, %RBX
    movq %RDI, %R12
    movq $0, %R13
    movq %RSI, %R14
    leaq .str_3, %R15
    jmp .lbl_20
.lbl_19:
    movq %RBX, %RDI
    movq %R12, %RSI
    call __concat__
    movq %RAX, %R15
    movq %R13, %RBX
    addq $1, %RBX
    movq %RBX, %R13
    movq %R15, %RBX
    jmp .lbl_20
.lbl_20:
    cmpq %R14, %R13
    jl .t_lbl_7
    movq %R15, %RBX
    jmp .lbl_21
.t_lbl_7:
    jmp .lbl_19
.lbl_21:
    movq %RBX, %RAX
    popq %R15
    popq %R14
    popq %R13
    popq %R12
    popq %RBX
    leave
    ret
