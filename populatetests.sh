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
for file in $tests
do
    get_test_name "$file"
    ./interpreter -ast < "$file" 2> "$testpath$ast_suffix"
    ./interpreter -sym < "$file" 2> "$testpath$sym_suffix"
done

echo "Tests are populated"

make clean
exit $had_failures