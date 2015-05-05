#!/bin/bash

for file in `find /Users/mabs/Research/tools/overload_checker/comparator_files -type f -name "*.java"` 
do
  /Users/mabs/Research/tools/overload_checker/triage/dist/build/triage/triage analyse $file
done
