#!/bin/bash

errc=0

fail() {
    cat build/fbuild.log
    errc=1
}

cd tests
./run_tests.py
cd ../examples
for dir in */; do
    [ $dir == "config/" -o $dir == "scala/" ] && continue
    echo "Running example $dir"
    cd $dir
    ../../fbuild-light || fail
    cd ..
done

exit $errc
