tests=$(find tests/intermedout -name *\.java)

get_test_name () {
    local fullpath=$1
    testpath="${fullpath%.*}"
    test_name="${testpath##*/}"
}

for file in $tests
do
    get_test_name "$file"

    cp $file javaclasses/Intermediate.java

    cd javaclasses
    javac Intermediate.java 2> ../tests/javacresults/$test_name.txt

    rm Intermediate.java

    cd ..

done