#!/bin/bash
if ! [ -e interpreter ]   
    then make all
fi

tests=$(find tests -name *\.corg)
had_failures="0"
ast_suffix=".astout"
sym_suffix=".symout"

get_test_name () {
    local fullpath=$1
    testpath="${fullpath%.*}"
    test_name="${testpath##*/}"
}

# Testing AST
echo "Testing Abstract Syntax Tree"
for file in $tests
do
    get_test_name "$file"
    ./interpreter -ast < "$file" 2> ".test_out"
    if [[ ! $(diff ".test_out" "$testpath$ast_suffix") ]]
    then
        echo "success: $test_name"
    else
        echo "FAIL:    $test_name"
        had_failures="1"

        printf "Expected: {\n"
        cat "$testpath$ast_suffix"
        printf "}\n"
        echo

        printf "Recieved: {\n"
        cat ".test_out"
        printf "}\n"
        echo
    fi
done

# Testing Symbol Tables
echo "Testing Symbol Tabling"
for file in $tests
do
    get_test_name "$file"
    ./interpreter -sym < "$file" 2> ".test_out"
    if [[ ! $(diff ".test_out" "$testpath$sym_suffix") ]]
    then
        echo "success: $test_name"
    else
        echo "FAIL:    $test_name"
        had_failures="1"

        printf "Expected: {\n"
        cat "$testpath$sym_suffix"
        printf "}\n"
        echo

        printf "Recieved: {\n"
        cat ".test_out"
        printf "}\n"
        echo
    fi
done


rm -f ".test_out"

make clean

exit $had_failures