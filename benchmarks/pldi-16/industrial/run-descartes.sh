#!/bin/bash

echo "ATTENTION: DESCARTES IN ACTION!"

echo "Property 1"
for f in *.java
do
	echo "Verifying $f"
        echo "Property 1" &> log/$f.log
	{ echo "descartes -p=1 -m=0: "; time descartes -p=1 -m=0 $f; } &>> log/$f.log
#	{ echo "descartes -p=1 -m=1: "; time descartes -p=1 -m=1 $f; } &>> log/$f-p1.log
#	{ echo "descartes -p=1 -m=2: "; time descartes -p=1 -m=2 $f; } &>> log/$f-p1.log
done

echo "Property 2"
for f in *.java
do
	echo "Verifying $f"
        echo "Property 2" &>> log/$f.log
	{ echo "descartes -p=2 -m=0: "; time descartes -p=2 -m=0 $f; } &>> log/$f.log
#	{ echo "descartes -p=2 -m=1: "; time descartes -p=2 -m=1 $f; } &>> log/$f-p2.log
#	{ echo "descartes -p=2 -m=2: "; time descartes -p=2 -m=2 $f; } &>> log/$f-p2.log
done

echo "Property 3"
for f in *.java
do
	echo "Verifying $f"
        echo "Property 3" &>> log/$f.log
	{ echo "descartes -p=3 -m=0: "; time descartes -p=3 -m=0 $f; } &>> log/$f.log
#	{ echo "descartes -p=3 -m=1: "; time descartes -p=3 -m=1 $f; } &>> log/$f-p3.log
#	{ echo "descartes -p=3 -m=2: "; time descartes -p=3 -m=2 $f; } &>> log/$f-p3.log
done
