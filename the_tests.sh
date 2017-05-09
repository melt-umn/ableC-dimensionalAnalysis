#!/bin/bash

# turn on option to exit on non-zero return code.
set -e
# turn on verbose option, which echos commands to stdout
set -v

cd artifact
./build.sh --clean

# this currently 'works' but reports errors and thus fails
cd ../examples
./compile.sh test.sh

./a.out

set +v

