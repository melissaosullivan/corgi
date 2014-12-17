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
for file in $tests
do
    get_test_name "$file"
    ./interpreter -ast < "$file" 2> "tests/$ast_outdir/$test_name$ast_suffix"
    ./interpreter -sym < "$file" 2> "tests/$sym_outdir/$test_name$sym_suffix"
    ./interpreter -javagen < "$file" 2> "tests/$intermed_outdir/$test_name$intermed_suffix"
done

echo "Tests are populated"

make clean
exit $had_failures