#!/bin/bash
set -e

# This script shows the steps in compiling an extended C program (.xc)
# down to C via an extended instance of ableC and then using GCC to
# compile the generated C code to an executable.
# Of course, if the use of 'cut' below fails for you, then just run
# the commands individually by hand.

# TODO: better handle dependency on cwd
top_level=".."
if [ -d ../../artifact ]; then
	top_level="../.."
elif [ -d ../../../artifact ]; then
	top_level="../../.."
fi

cmd="java -jar $top_level/artifact/ableC.jar $1 -I $top_level/include"
echo $cmd
$cmd

# extract the base filename, everything before the dot (.)

filename=$1
extension="${filename##*.}"
filename_withoutpath=$(basename $filename)
basefilename="${filename_withoutpath%.*}"

cfile="${basefilename}.pp_out.c"
#cfile="${basefilename}.c"

cmd="gcc ${link_sqlite3} ${cfile} -o ${basefilename}"
echo $cmd
$cmd

