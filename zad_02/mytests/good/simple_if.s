main:
    pushq %RBP
    movq %RSP, %RBP
    pushq %RBX
    pushq %R12
    pushq %R13
    pushq %R14
    pushq %R15
    subq $-8, %RSP
    cmpq $1, $0
    jg .lbl_0
    jmp .lbl_1
.lbl_0:
    movq $10, %RDI
    call printInt
    jmp .lbl_2
.lbl_1:
    movq $20, %RDI
    call printInt
    jmp .lbl_2
.lbl_2:
    movq $0, %RAX
    popq %R15
    popq %R14
    popq %R13
    popq %R12
    popq %RBX
    leave
    ret
