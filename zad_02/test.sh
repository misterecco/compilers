#!/bin/sh

FAILED=0

test_good () {
    SRC=$1.lat
    EXEC=$1
    INPUT=$1.input
    OUTPUT=$1.output
    EXEC_OUT=$1.out

    ./latc_x86_64 $SRC 2> /dev/null

    if [ -f $INPUT ]; then
        ./$EXEC < $INPUT > $EXEC_OUT 
    else
        ./$EXEC > $EXEC_OUT
    fi

    diff -b $OUTPUT $EXEC_OUT > /dev/null
    if [ $? -eq 0 ]; then
        rm $EXEC_OUT
    else
        (( FAILED++ ))
        echo "$1 FAILED!"                
    fi

    rm $EXEC
}

test_bad () {
    SRC=$1.lat
    OUT=$(./latc_x86_64 $SRC 2>&1 >/dev/null | head -n 1)

    if [ "$OUT" != "ERROR" ]; then
        (( FAILED++ ))
        echo "$1 FAILED"
    fi
}

run_tests () {
    DIR=$1
    FAILED=0

    echo "-------------------------------------------"
    echo "Running tests from directory $DIR"

    TESTS=$(ls $DIR | grep .lat | cut -f 1 -d '.')

    for TEST in $TESTS 
    do
        $2 $DIR/$TEST
    done

    if [ $FAILED -eq 0 ]; then
        echo "All OK!"
    else
        echo "$FAILED tests failed!"
    fi

    echo "-------------------------------------------"    
}

run_tests lattests/good test_good
run_tests lattests/bad test_bad
run_tests mytests/good test_good
run_tests mytests/bad test_bad
