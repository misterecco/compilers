main:
    pushq %RBP
    movq %RSP, %RBP
    pushq %RBX
    pushq %R12
    pushq %R13
    pushq %R14
    pushq %R15
    subq $-8, %RSP
    call readInt
    movq %RAX, %RBX
    cmpq $10, %RBX
    jg .lbl_0
    jmp .lbl_1
.lbl_0:
    movq %RBX, %R12
    jmp .lbl_2
.lbl_1:
    movq $10, %RBX
    movq $5, %R12
    jmp .lbl_2
.lbl_2:
    movq %RBX, %RDI
    call printInt
    movq %R12, %RDI
    call printInt
    movq $0, %RAX
    popq %R15
    popq %R14
    popq %R13
    popq %R12
    popq %RBX
    leave
    ret
