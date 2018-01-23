.section .data
.str_2:
    .string "!"
.str_0:
    .string "&&"
.str_3:
    .string "false"
.str_4:
    .string "true"
.str_1:
    .string "||"
.section .text
    .p2align 4,,15
    .globl main
    .type main, @function
main:
    pushq %RBP
    movq %RSP, %RBP
    pushq %RBX
    pushq %R12
    leaq .str_0, %RDI
    call printString
    movq $1, %RBX
    imulq $-1, %RBX
    movq %RBX, %RDI
    call test
    movq %RAX, %R12
    cmpq $1, %R12
    je .t_lbl_0
    jmp .lbl_1
.t_lbl_0:
    jmp .lbl_2
.lbl_2:
    movq $0, %RDI
    call test
    movq %RAX, %RBX
    cmpq $1, %RBX
    je .t_lbl_1
    jmp .lbl_1
.t_lbl_1:
    jmp .lbl_0
.lbl_0:
    movq $1, %RBX
    jmp .lbl_3
.lbl_1:
    movq $0, %RBX
    jmp .lbl_3
.lbl_3:
    movq %RBX, %RDI
    call printBool
    movq $2, %RBX
    imulq $-1, %RBX
    movq %RBX, %RDI
    call test
    movq %RAX, %R12
    cmpq $1, %R12
    je .t_lbl_2
    jmp .lbl_5
.t_lbl_2:
    jmp .lbl_6
.lbl_6:
    movq $1, %RDI
    call test
    movq %RAX, %RBX
    cmpq $1, %RBX
    je .t_lbl_3
    jmp .lbl_5
.t_lbl_3:
    jmp .lbl_4
.lbl_4:
    movq $1, %RBX
    jmp .lbl_7
.lbl_5:
    movq $0, %RBX
    jmp .lbl_7
.lbl_7:
    movq %RBX, %RDI
    call printBool
    movq $3, %RDI
    call test
    movq %RAX, %RBX
    cmpq $1, %RBX
    je .t_lbl_4
    jmp .lbl_9
.t_lbl_4:
    jmp .lbl_10
.lbl_10:
    movq $5, %RBX
    imulq $-1, %RBX
    movq %RBX, %RDI
    call test
    movq %RAX, %R12
    cmpq $1, %R12
    je .t_lbl_5
    jmp .lbl_9
.t_lbl_5:
    jmp .lbl_8
.lbl_8:
    movq $1, %RBX
    jmp .lbl_11
.lbl_9:
    movq $0, %RBX
    jmp .lbl_11
.lbl_11:
    movq %RBX, %RDI
    call printBool
    movq $234234, %RDI
    call test
    movq %RAX, %RBX
    cmpq $1, %RBX
    je .t_lbl_6
    jmp .lbl_13
.t_lbl_6:
    jmp .lbl_14
.lbl_14:
    movq $21321, %RDI
    call test
    movq %RAX, %RBX
    cmpq $1, %RBX
    je .t_lbl_7
    jmp .lbl_13
.t_lbl_7:
    jmp .lbl_12
.lbl_12:
    movq $1, %RBX
    jmp .lbl_15
.lbl_13:
    movq $0, %RBX
    jmp .lbl_15
.lbl_15:
    movq %RBX, %RDI
    call printBool
    leaq .str_1, %RDI
    call printString
    movq $1, %RBX
    imulq $-1, %RBX
    movq %RBX, %RDI
    call test
    movq %RAX, %R12
    cmpq $1, %R12
    je .t_lbl_8
    jmp .lbl_18
.t_lbl_8:
    jmp .lbl_16
.lbl_18:
    movq $0, %RDI
    call test
    movq %RAX, %RBX
    cmpq $1, %RBX
    je .t_lbl_9
    jmp .lbl_17
.t_lbl_9:
    jmp .lbl_16
.lbl_16:
    movq $1, %RBX
    jmp .lbl_19
.lbl_17:
    movq $0, %RBX
    jmp .lbl_19
.lbl_19:
    movq %RBX, %RDI
    call printBool
    movq $2, %RBX
    imulq $-1, %RBX
    movq %RBX, %RDI
    call test
    movq %RAX, %R12
    cmpq $1, %R12
    je .t_lbl_10
    jmp .lbl_22
.t_lbl_10:
    jmp .lbl_20
.lbl_22:
    movq $1, %RDI
    call test
    movq %RAX, %RBX
    cmpq $1, %RBX
    je .t_lbl_11
    jmp .lbl_21
.t_lbl_11:
    jmp .lbl_20
.lbl_20:
    movq $1, %RBX
    jmp .lbl_23
.lbl_21:
    movq $0, %RBX
    jmp .lbl_23
.lbl_23:
    movq %RBX, %RDI
    call printBool
    movq $3, %RDI
    call test
    movq %RAX, %RBX
    cmpq $1, %RBX
    je .t_lbl_12
    jmp .lbl_26
.t_lbl_12:
    jmp .lbl_24
.lbl_26:
    movq $5, %RBX
    imulq $-1, %RBX
    movq %RBX, %RDI
    call test
    movq %RAX, %R12
    cmpq $1, %R12
    je .t_lbl_13
    jmp .lbl_25
.t_lbl_13:
    jmp .lbl_24
.lbl_24:
    movq $1, %RBX
    jmp .lbl_27
.lbl_25:
    movq $0, %RBX
    jmp .lbl_27
.lbl_27:
    movq %RBX, %RDI
    call printBool
    movq $234234, %RDI
    call test
    movq %RAX, %RBX
    cmpq $1, %RBX
    je .t_lbl_14
    jmp .lbl_30
.t_lbl_14:
    jmp .lbl_28
.lbl_30:
    movq $21321, %RDI
    call test
    movq %RAX, %RBX
    cmpq $1, %RBX
    je .t_lbl_15
    jmp .lbl_29
.t_lbl_15:
    jmp .lbl_28
.lbl_28:
    movq $1, %RBX
    jmp .lbl_31
.lbl_29:
    movq $0, %RBX
    jmp .lbl_31
.lbl_31:
    movq %RBX, %RDI
    call printBool
    leaq .str_2, %RDI
    call printString
    movq $1, %RDI
    call printBool
    movq $0, %RDI
    call printBool
    movq $0, %RAX
    popq %R12
    popq %RBX
    leave
    ret
printBool:
    pushq %RBP
    movq %RSP, %RBP
    addq $-8, %RSP
    pushq %RBX
    cmpq $1, %RDI
    je .t_lbl_16
    jmp .lbl_32
.t_lbl_16:
    jmp .lbl_33
.lbl_32:
    leaq .str_3, %RDI
    call printString
    jmp .lbl_34
.lbl_33:
    leaq .str_4, %RDI
    call printString
    jmp .lbl_34
.lbl_34:
    popq %RBX
    leave
    ret
test:
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
    cmpq $0, %RDI
    jg .t_lbl_17
    jmp .lbl_36
.t_lbl_17:
    jmp .lbl_35
.lbl_35:
    movq $1, %RBX
    jmp .lbl_37
.lbl_36:
    movq $0, %RBX
    jmp .lbl_37
.lbl_37:
    movq %RBX, %RAX
    popq %RBX
    leave
    ret
