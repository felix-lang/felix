#!/bin/bash

errc=0

fail() {
    echo "Example $1 FAILED; showing log"
    cat build/fbuild.log
    errc=1
}

cd tests
./run_tests.py
cd ../examples
for dir in */; do
    [ $dir == "config/" -o $dir == "scala/" -o $dir == "ocaml-batteries/" ] && continue
    echo "Running example $dir"
    cd $dir
    ../../fbuild-light || fail $dir
    cd ..
done

exit $errc
