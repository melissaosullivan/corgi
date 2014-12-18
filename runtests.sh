#!/bin/bash
if ! [ -e interpreter ]   
    then make all
fi

tests=$(find tests -name *\.corgi)
had_failures="0"
ast_suffix=".astout"
sym_suffix=".symout"
sem_suffix=".semout"
intermed_suffix=".java"

ast_outdir="astout"
sym_outdir="symout"
sem_outdir="semout"
intermed_outdir="intermedout"
final_outdir="finalout"

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
        cat "tests/$ast_outdir/$test_name$ast_suffix"
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
        cat "tests/$sym_outdir/$test_name$sym_suffix"
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
echo "----------------Testing Semantic Checking Output----------------"
echo ""
for file in $tests
do
    get_test_name "$file"
    ./interpreter -sem < "$file" 2> ".test_out"
    if [[ ! $(diff ".test_out" "tests/$sem_outdir/$test_name$sem_suffix") ]]
    then
        echo "success: $test_name"
    else
        echo "FAIL:    $test_name"
        had_failures="1"

        printf "Expected: {\n"
        cat "tests/$sem_outdir/$test_name$sem_suffix"
        printf "}\n"
        echo

        printf "Recieved: {\n"
        cat ".test_out"
        printf "}\n"
        echo
    fi
done


# Testing Final Output
: '
echo ""
echo "----------------Testing Final Output----------------"
echo ""
for file in $tests
do
    get_test_name "$file"
    ./interpreter -javagen < "$file" 2> ".test_out"

    cp .test_out javaclasses/Intermediate.java

    cd javaclasses
    javac Intermediate.java
    java Intermediate > ../.test_out

    rm Intermediate.java
    rm Intermediate.class

    cd ..

    if [[ ! $(diff ".test_out" "tests/$final_outdir/$test_name.txt") ]]
    then
        echo "success: $test_name"
    else
        echo "FAIL:    $test_name"
        had_failures="1"

        printf "Expected: {\n"
        cat "tests/$final_outdir/$test_name.txt"
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