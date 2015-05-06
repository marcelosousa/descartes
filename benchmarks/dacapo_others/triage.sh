#!/bin/bash

for file in `find /Users/mabs/Research/tools/descartes/benchmarks/dacapo_others -type f -name "*.java"` 
do
  /Users/mabs/Research/tools/descartes/triage/dist/build/triage/triage analyse $file
done
