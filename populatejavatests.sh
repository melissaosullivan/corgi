
tests=$(find tests/intermedout -name *\.java)

get_test_name () {
    local fullpath=$1
    testpath="${fullpath%.*}"
    test_name="${testpath##*/}"
}

echo ""

for file in $tests
do
    get_test_name "$file"
    
    cp $file javaclasses/Intermediate.java

    cd javaclasses

    echo "Compiliing"
    javac Intermediate.java
    java Intermediate > ../tests/finalout/$test_name.txt

    rm Intermediate.java
    rm Intermediate.class

    cd ..

done