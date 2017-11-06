@dnl = internal constant [4 x i8] c"%d\0A\00"
declare i32 @printf(i8*, ...)

define void @printInt(i32 %x) {
    %t = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0
    call i32 (i8*, ...) @printf(i8* %t, i32 %x)
    ret void
}


define i32 @main() {
    %loc_a = alloca i32
    store i32 0, i32* %loc_a
    %loc_b = alloca i32
    store i32 1, i32* %loc_b
    %loc_c = alloca i32
    store i32 0, i32* %loc_c
    %loc_d = alloca i32
    store i32 1, i32* %loc_d
    %loc_e = alloca i32
    store i32 0, i32* %loc_e
    %loc_f = alloca i32
    store i32 1, i32* %loc_f
    %loc_g = alloca i32
    store i32 0, i32* %loc_g
    %loc_h = alloca i32
    store i32 1, i32* %loc_h
    %t_0 = load i32, i32* %loc_a
    %t_1 = load i32, i32* %loc_b
    %t_2 = mul i32 %t_0, %t_1
    %t_3 = load i32, i32* %loc_c
    %t_4 = load i32, i32* %loc_d
    %t_5 = mul i32 %t_3, %t_4
    %t_6 = load i32, i32* %loc_e
    %t_7 = load i32, i32* %loc_f
    %t_8 = load i32, i32* %loc_g
    %t_9 = load i32, i32* %loc_h
    %t_10 = add i32 %t_8, %t_9
    %t_11 = add i32 %t_7, %t_10
    %t_12 = add i32 %t_6, %t_11
    %t_13 = add i32 %t_5, %t_12
    %t_14 = add i32 %t_2, %t_13
    call void @printInt(i32 %t_14)
    store i32 1, i32* %loc_a
    store i32 2, i32* %loc_b
    store i32 1, i32* %loc_c
    store i32 2, i32* %loc_d
    store i32 1, i32* %loc_e
    store i32 2, i32* %loc_f
    store i32 1, i32* %loc_g
    store i32 2, i32* %loc_h
    %loc_i = alloca i32
    store i32 1, i32* %loc_i
    %loc_j = alloca i32
    store i32 2, i32* %loc_j
    %loc_k = alloca i32
    store i32 1, i32* %loc_k
    %loc_l = alloca i32
    store i32 2, i32* %loc_l
    %loc_m = alloca i32
    store i32 1, i32* %loc_m
    %loc_n = alloca i32
    store i32 2, i32* %loc_n
    %t_15 = load i32, i32* %loc_a
    %t_16 = mul i32 2, %t_15
    %t_17 = load i32, i32* %loc_b
    %t_18 = sdiv i32 %t_17, 2
    %t_19 = load i32, i32* %loc_c
    %t_20 = load i32, i32* %loc_d
    %t_21 = load i32, i32* %loc_e
    %t_22 = load i32, i32* %loc_f
    %t_23 = load i32, i32* %loc_g
    %t_24 = load i32, i32* %loc_h
    %t_25 = load i32, i32* %loc_i
    %t_26 = load i32, i32* %loc_j
    %t_27 = sdiv i32 %t_26, 2
    %t_28 = load i32, i32* %loc_k
    %t_29 = load i32, i32* %loc_l
    %t_30 = load i32, i32* %loc_m
    %t_31 = load i32, i32* %loc_n
    %t_32 = add i32 %t_30, %t_31
    %t_33 = add i32 %t_29, %t_32
    %t_34 = add i32 %t_28, %t_33
    %t_35 = add i32 %t_27, %t_34
    %t_36 = add i32 %t_25, %t_35
    %t_37 = add i32 %t_24, %t_36
    %t_38 = add i32 %t_23, %t_37
    %t_39 = add i32 %t_22, %t_38
    %t_40 = add i32 %t_21, %t_39
    %t_41 = add i32 %t_20, %t_40
    %t_42 = add i32 %t_19, %t_41
    %t_43 = add i32 %t_18, %t_42
    %t_44 = add i32 %t_16, %t_43
    %t_45 = sdiv i32 %t_44, 10
    call void @printInt(i32 %t_45)
  ret i32 0
}
