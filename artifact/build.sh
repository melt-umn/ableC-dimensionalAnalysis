#!/bin/bash

# Since this sample artifact directory is just named 'artifact' the script
# for building this one and all others are the same.

# The -I directive include, respectively, the parent directory of the
# artifact, skeleton, and ableC specification direcoties.

silver  -I ../.. -I ../../../ableC -o ableC.jar $@ edu:umn:cs:melt:exts:ableC:dimensionalAnalysis:artifact

rm -f build.xml
