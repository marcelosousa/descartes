#!/bin/bash

# { echo "descartes Prop 1:"; time descartes -p=1 CollectionItem-false.java; } &> log/CollectionItem-false-p1.log
# { echo "descartes Prop 2:"; time descartes -p=2 CollectionItem-false.java; } &> log/CollectionItem-false-p2.log
# { echo "descartes Prop 3:"; time descartes -p=3 CollectionItem-false.java; } &> log/CollectionItem-false-p3.log
# { echo "descartes Prop 1:"; time descartes -p=1 CollectionItem-true.java; } &> log/CollectionItem-true-p1.log
# { echo "descartes Prop 2:"; time descartes -p=2 CollectionItem-true.java; } &> log/CollectionItem-true-p2.log
# { echo "descartes Prop 3:"; time descartes -p=3 CollectionItem-true.java; } &> log/CollectionItem-true-p3.log

{ echo "descartes Prop 1:"; time descartes -p=1 Date-false.java; } &> log/Date-false-p1.log
{ echo "descartes Prop 2:"; time descartes -p=2 Date-false.java; } &> log/Date-false-p2.log
{ echo "descartes Prop 3:"; time descartes -p=3 Date-false.java; } &> log/Date-false-p3.log
{ echo "descartes Prop 1:"; time descartes -p=1 Date-true.java; } &> log/Date-true-p1.log
{ echo "descartes Prop 2:"; time descartes -p=2 Date-true.java; } &> log/Date-true-p2.log
{ echo "descartes Prop 3:"; time descartes -p=3 Date-true.java; } &> log/Date-true-p3.log
