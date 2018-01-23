.section .data
.str_0:
    .string "apa"
.str_2:
    .string "false"
.str_1:
    .string "true"
.section .text
    .p2align 4,,15
    .globl main
    .type main, @function
main:
    pushq %RBP
    movq %RSP, %RBP
    pushq %RBX
    pushq %R12
    movq $3, %RAX
    cmpq $4, %RAX
    jle .t_lbl_0
    jmp .lbl_1
.t_lbl_0:
    jmp .lbl_3
.lbl_3:
    movq $4, %RAX
    cmpq $2, %RAX
    jne .t_lbl_2
    jmp .lbl_1
.t_lbl_2:
    jmp .lbl_4
.lbl_4:
    jmp .lbl_0
.lbl_0:
    movq $1, %RDI
    call printBool
    jmp .lbl_2
.lbl_1:
    leaq .str_0, %RDI
    call printString
    jmp .lbl_2
.lbl_2:
    movq $1, %RAX
    cmpq $1, %RAX
    je .t_lbl_1
    jmp .lbl_7
.t_lbl_1:
    jmp .lbl_5
.lbl_7:
    movq $1, %RDI
    call dontCallMe
    movq %RAX, %RBX
    cmpq $1, %RBX
    je .t_lbl_3
    jmp .lbl_6
.t_lbl_3:
    jmp .lbl_5
.lbl_5:
    movq $1, %RBX
    jmp .lbl_8
.lbl_6:
    movq $0, %RBX
    jmp .lbl_8
.lbl_8:
    movq %RBX, %RDI
    call printBool
    movq $5, %RBX
    imulq $-1, %RBX
    movq $4, %RAX
    cmpq %RBX, %RAX
    jl .t_lbl_4
    jmp .lbl_10
.t_lbl_4:
    jmp .lbl_11
.lbl_11:
    movq $2, %RDI
    call dontCallMe
    movq %RAX, %RBX
    cmpq $1, %RBX
    je .t_lbl_5
    jmp .lbl_10
.t_lbl_5:
    jmp .lbl_9
.lbl_9:
    movq $1, %RBX
    movq $4, %R12
    jmp .lbl_12
.lbl_10:
    movq $0, %RBX
    movq $4, %R12
    jmp .lbl_12
.lbl_12:
    movq %RBX, %RDI
    call printBool
    movq $4, %RAX
    cmpq %R12, %RAX
    je .t_lbl_6
    jmp .lbl_14
.t_lbl_6:
    jmp .lbl_15
.lbl_15:
    movq $1, %RBX
    subq $0, %RBX
    movq $1, %RAX
    cmpq %RBX, %RAX
    je .t_lbl_7
    jmp .lbl_14
.t_lbl_7:
    jmp .lbl_16
.lbl_16:
    jmp .lbl_13
.lbl_13:
    movq $1, %RBX
    jmp .lbl_17
.lbl_14:
    movq $0, %RBX
    jmp .lbl_17
.lbl_17:
    movq %RBX, %RDI
    call printBool
    movq $0, %RDI
    movq $0, %RSI
    call implies
    movq %RAX, %RBX
    movq %RBX, %RDI
    call printBool
    movq $0, %RDI
    movq $1, %RSI
    call implies
    movq %RAX, %RBX
    movq %RBX, %RDI
    call printBool
    movq $1, %RDI
    movq $0, %RSI
    call implies
    movq %RAX, %RBX
    movq %RBX, %RDI
    call printBool
    movq $1, %RDI
    movq $1, %RSI
    call implies
    movq %RAX, %RBX
    movq %RBX, %RDI
    call printBool
    movq $0, %RAX
    popq %R12
    popq %RBX
    leave
    ret
dontCallMe:
    pushq %RBP
    movq %RSP, %RBP
    addq $-8, %RSP
    pushq %RBX
    pushq %RDI
    subq $8, %RSP
    movq 8(%RSP), %RDI
    call printInt
    addq $8, %RSP
    popq %RDI
    movq $1, %RAX
    popq %RBX
    leave
    ret
printBool:
    pushq %RBP
    movq %RSP, %RBP
    addq $-8, %RSP
    pushq %RBX
    cmpq $1, %RDI
    je .t_lbl_8
    jmp .lbl_19
.t_lbl_8:
    jmp .lbl_18
.lbl_18:
    leaq .str_1, %RDI
    call printString
    jmp .lbl_20
.lbl_19:
    leaq .str_2, %RDI
    call printString
    jmp .lbl_20
.lbl_20:
    popq %RBX
    leave
    ret
implies:
    pushq %RBP
    movq %RSP, %RBP
    pushq %RBX
    pushq %R12
    cmpq $1, %RDI
    je .t_lbl_9
    jmp .lbl_21
.t_lbl_9:
    movq %RDI, %RBX
    movq %RSI, %R12
    jmp .lbl_23
.lbl_23:
    cmpq %R12, %RBX
    je .t_lbl_10
    jmp .lbl_22
.t_lbl_10:
    jmp .lbl_21
.lbl_21:
    movq $1, %RBX
    jmp .lbl_24
.lbl_22:
    movq $0, %RBX
    jmp .lbl_24
.lbl_24:
    movq %RBX, %RAX
    popq %R12
    popq %RBX
    leave
    ret
