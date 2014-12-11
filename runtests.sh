#!/bin/bash

tests=$(find tests -name *\.corg)
had_failures="0"
suffix=".out"

get_test_name () {
    local fullpath=$1
    testpath="${fullpath%.*}"
    test_name="${testpath##*/}"

}

for file in $tests
do
    get_test_name "$file"

    ./interpret < "$file" 2> ".test_out"

    if [[ ! $(diff ".test_out" "$testpath$suffix") ]]
    then
        echo "success: $test_name"
    else
        echo "FAIL:    $test_name"
        had_failures="1"

        printf "Expected: {\n"
        cat "$testpath$suffix"
        printf "}\n"
        echo

        printf "Recieved: {\n"
        cat ".test_out"
        printf "}\n"
        echo
    fi
done

rm -f ".test_out"
exit $had_failures