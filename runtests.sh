#!/bin/bash
if ! [ -e interpreter ]   
    then make all
fi

tests=$(find tests -name *\.corg)
had_failures="0"
ast_suffix=".astout"
sym_suffix=".symout"
intermed_suffix=".java"

ast_outdir="astout"
sym_outdir="symout"
intermed_outdir="intermedout"

get_test_name () {
    local fullpath=$1
    testpath="${fullpath%.*}"
    test_name="${testpath##*/}"
}

# Testing AST
echo ""
echo "----------------Testing Abstract Syntax Tree Output----------------"
echo ""

for file in $tests
do
    get_test_name "$file"
    ./interpreter -ast < "$file" 2> ".test_out"
    if [[ ! $(diff ".test_out" "tests/$ast_outdir/$test_name$ast_suffix") ]]
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
echo ""
echo "----------------Testing Symbol Table Output----------------"
echo ""
for file in $tests
do
    get_test_name "$file"
    ./interpreter -sym < "$file" 2> ".test_out"
    if [[ ! $(diff ".test_out" "tests/$sym_outdir/$test_name$sym_suffix") ]]
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


# Testing Java Output
: ' Skip the Java tests for now.
echo ""
echo "----------------Testing Intermediate Output----------------"
echo ""
for file in $tests
do
    get_test_name "$file"
    ./interpreter -javagen < "$file" 2> ".test_out"
    if [[ ! $(diff ".test_out" "tests/$intermed_outdir/$test_name$intermed_suffix") ]]
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
'


echo ""
echo "----------------Finished Testing, Running Make Clean----------------"
echo ""


rm -f ".test_out"

make clean

exit $had_failures