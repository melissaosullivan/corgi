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

# Remove all previous test results
rm -rf tests/$ast_outdir/*
rm -rf tests/$sym_outdir/*
rm -rf tests/$intermed_outdir/*
rm -rf tests/$final_outdir/*


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
    ./interpreter -sem < "$file" 2> "tests/$sem_outdir/$test_name$sem_suffix"
    ./interpreter -javagen < "$file" 2> "tests/$intermed_outdir/$test_name$intermed_suffix"
done

# Populate the java output tests
# ./populatejavatests.sh


echo "Tests are populated"

make clean
exit $had_failures