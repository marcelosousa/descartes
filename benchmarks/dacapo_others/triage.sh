#!/bin/bash

for file in `find /Users/mabs/Research/tools/overload_checker/ex01/test/src -type f -name "*.java"` 
do
  /Users/mabs/Research/tools/overload_checker/triage/dist/build/triage/triage analyse $file
done
