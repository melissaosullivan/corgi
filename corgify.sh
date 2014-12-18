#!/bin/bash

# Make if not made yet
if ! [ -e interpreter ]   
    then make all
fi

./interpreter -javagen < $1 2> javaclasses/Intermediate.java
# shift
cd javaclasses

echo ""
echo "Compiling"

javac Intermediate.java

java Intermediate

rm Intermediate.class
rm Intermediate.java

cd ..