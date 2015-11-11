#!/bin/bash

echo "ATTENTION: DESCARTES IN ACTION!"

echo "Property 4 (Transitivity)"
for f in *.java
do
	echo "Verifying $f"
  echo "Property 4 (Transitivity)" &> log/$f-p4.log
	{ echo "descartes -p=4 -m=0: "; time descartes -p=4 -m=0 $f; } &>> log/$f-p4.log
#	{ echo "descartes -p=4 -m=1: "; time descartes -p=4 -m=1 $f; } &>> log/$f-p4.log
#	{ echo "descartes -p=4 -m=2: "; time descartes -p=4 -m=2 $f; } &>> log/$f-p4.log
#	{ echo "descartes -p=4 -m=3: "; time descartes -p=4 -m=3 $f; } &>> log/$f-p4.log
done

echo "Property 5 (Symmetry)"
for f in *.java
do
	echo "Verifying $f"
  echo "Property 5 (Symmetry)" &> log/$f-p5.log
	{ echo "descartes -p=5 -m=0: "; time descartes -p=5 -m=0 $f; } &>> log/$f-p5.log
#	{ echo "descartes -p=5 -m=1: "; time descartes -p=5 -m=1 $f; } &>> log/$f-p5.log
#	{ echo "descartes -p=5 -m=2: "; time descartes -p=5 -m=2 $f; } &>> log/$f-p5.log
#	{ echo "descartes -p=5 -m=3: "; time descartes -p=5 -m=3 $f; } &>> log/$f-p5.log
done

echo "Property 6 (Consistency)"
for f in *.java
do
	echo "Verifying $f"
  echo "Property 6 (Consistency)" &> log/$f-p6.log
	{ echo "descartes -p=6 -m=0: "; time descartes -p=6 -m=0 $f; } &>> log/$f-p6.log
#	{ echo "descartes -p=6 -m=1: "; time descartes -p=6 -m=1 $f; } &>> log/$f-p6.log
#	{ echo "descartes -p=6 -m=2: "; time descartes -p=6 -m=2 $f; } &>> log/$f-p6.log
#	{ echo "descartes -p=6 -m=3: "; time descartes -p=6 -m=3 $f; } &>> log/$f-p6.log
done
