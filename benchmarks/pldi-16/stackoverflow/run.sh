#!/bin/bash

 { echo "descartes Prop 1:"; time descartes -p=1 CollectionItem-false.java; } &> log/CollectionItem-false-p1.log
 { echo "descartes Prop 2:"; time descartes -p=2 CollectionItem-false.java; } &> log/CollectionItem-false-p2.log
 { echo "descartes Prop 3:"; time descartes -p=3 CollectionItem-false.java; } &> log/CollectionItem-false-p3.log
 { echo "descartes Prop 1:"; time descartes -p=1 CollectionItem-true.java; } &> log/CollectionItem-true-p1.log
 { echo "descartes Prop 2:"; time descartes -p=2 CollectionItem-true.java; } &> log/CollectionItem-true-p2.log
 { echo "descartes Prop 3:"; time descartes -p=3 CollectionItem-true.java; } &> log/CollectionItem-true-p3.log

# { echo "descartes Prop 1:"; time descartes -p=1 Date-false.java; } &> log/Date-false-p1.log
# { echo "descartes Prop 2:"; time descartes -p=2 Date-false.java; } &> log/Date-false-p2.log
# { echo "descartes Prop 3:"; time descartes -p=3 Date-false.java; } &> log/Date-false-p3.log
# { echo "descartes Prop 1:"; time descartes -p=1 Date-true.java; } &> log/Date-true-p1.log
# { echo "descartes Prop 2:"; time descartes -p=2 Date-true.java; } &> log/Date-true-p2.log
# { echo "descartes Prop 3:"; time descartes -p=3 Date-true.java; } &> log/Date-true-p3.log
                                                                                           

# { echo "descartes Prop 1:"; time descartes -p=1 ExportTerm-false.java; } &> log/ExportTerm-false-p1.log
# { echo "descartes Prop 2:"; time descartes -p=2 ExportTerm-false.java; } &> log/ExportTerm-false-p2.log
# { echo "descartes Prop 3:"; time descartes -p=3 ExportTerm-false.java; } &> log/ExportTerm-false-p3.log
# { echo "descartes Prop 1:"; time descartes -p=1 ExportTerm-true.java; }  &> log/ExportTerm-true-p1.log
# { echo "descartes Prop 2:"; time descartes -p=2 ExportTerm-true.java; }  &> log/ExportTerm-true-p2.log
# { echo "descartes Prop 3:"; time descartes -p=3 ExportTerm-true.java; }  &> log/ExportTerm-true-p3.log

# { echo "descartes Prop 1:"; time descartes -p=1 FileItem-false.java; } &> log/FileItem-false-p1.log
# { echo "descartes Prop 2:"; time descartes -p=2 FileItem-false.java; } &> log/FileItem-false-p2.log
# { echo "descartes Prop 3:"; time descartes -p=3 FileItem-false.java; } &> log/FileItem-false-p3.log
# { echo "descartes Prop 1:"; time descartes -p=1 FileItem-true.java; }  &> log/FileItem-true-p1.log
# { echo "descartes Prop 2:"; time descartes -p=2 FileItem-true.java; }  &> log/FileItem-true-p2.log
# { echo "descartes Prop 3:"; time descartes -p=3 FileItem-true.java; }  &> log/FileItem-true-p3.log

# { echo "descartes Prop 1:"; time descartes -p=1 MapValueComparator-false.java; } &> log/MapValueComparator-false-p1.log
# { echo "descartes Prop 2:"; time descartes -p=2 MapValueComparator-false.java; } &> log/MapValueComparator-false-p2.log
# { echo "descartes Prop 3:"; time descartes -p=3 MapValueComparator-false.java; } &> log/MapValueComparator-false-p3.log
# { echo "descartes Prop 1:"; time descartes -p=1 MapValueComparator-true.java; }  &> log/MapValueComparator-true-p1.log
# { echo "descartes Prop 2:"; time descartes -p=2 MapValueComparator-true.java; }  &> log/MapValueComparator-true-p2.log
# { echo "descartes Prop 3:"; time descartes -p=3 MapValueComparator-true.java; }  &> log/MapValueComparator-true-p3.log

# { echo "descartes Prop 1:"; time descartes -p=1 Node-false.java; } &> log/Node-false-p1.log
# { echo "descartes Prop 2:"; time descartes -p=2 Node-false.java; } &> log/Node-false-p2.log
# { echo "descartes Prop 3:"; time descartes -p=3 Node-false.java; } &> log/Node-false-p3.log
# { echo "descartes Prop 1:"; time descartes -p=1 Node-true.java; }  &> log/Node-true-p1.log
# { echo "descartes Prop 2:"; time descartes -p=2 Node-true.java; }  &> log/Node-true-p2.log
# { echo "descartes Prop 3:"; time descartes -p=3 Node-true.java; }  &> log/Node-true-p3.log

# { echo "descartes Prop 1:"; time descartes -p=1 NzbFile-false.java; } &> log/NzbFile-false-p1.log
# { echo "descartes Prop 2:"; time descartes -p=2 NzbFile-false.java; } &> log/NzbFile-false-p2.log
# { echo "descartes Prop 3:"; time descartes -p=3 NzbFile-false.java; } &> log/NzbFile-false-p3.log
# FIX
# { echo "descartes Prop 1:"; time descartes -p=1 NzbFile-true.java; }  &> log/NzbFile-true-p1.log
# { echo "descartes Prop 2:"; time descartes -p=2 NzbFile-true.java; }  &> log/NzbFile-true-p2.log
# { echo "descartes Prop 3:"; time descartes -p=3 NzbFile-true.java; }  &> log/NzbFile-true-p3.log

# { echo "descartes Prop 1:"; time descartes -p=1 Person-false.java; } &> log/Person-false-p1.log
# { echo "descartes Prop 2:"; time descartes -p=2 Person-false.java; } &> log/Person-false-p2.log
# { echo "descartes Prop 3:"; time descartes -p=3 Person-false.java; } &> log/Person-false-p3.log

# { echo "descartes Prop 1:"; time descartes -p=1 SimpleString-false.java; } &> log/SimpleString-false-p1.log
# { echo "descartes Prop 2:"; time descartes -p=2 SimpleString-false.java; } &> log/SimpleString-false-p2.log
# { echo "descartes Prop 3:"; time descartes -p=3 SimpleString-false.java; } &> log/SimpleString-false-p3.log
# { echo "descartes Prop 1:"; time descartes -p=1 SimpleString-true.java; }  &> log/SimpleString-true-p1.log
# { echo "descartes Prop 2:"; time descartes -p=2 SimpleString-true.java; }  &> log/SimpleString-true-p2.log
# { echo "descartes Prop 3:"; time descartes -p=3 SimpleString-true.java; }  &> log/SimpleString-true-p3.log

# { echo "descartes Prop 1:"; time descartes -p=1 Sre-false.java; } &> log/Sre-false-p1.log
# { echo "descartes Prop 2:"; time descartes -p=2 Sre-false.java; } &> log/Sre-false-p2.log
# { echo "descartes Prop 3:"; time descartes -p=3 Sre-false.java; } &> log/Sre-false-p3.log
# { echo "descartes Prop 1:"; time descartes -p=1 Sre-true.java; }  &> log/Sre-true-p1.log
# { echo "descartes Prop 2:"; time descartes -p=2 Sre-true.java; }  &> log/Sre-true-p2.log
# { echo "descartes Prop 3:"; time descartes -p=3 Sre-true.java; }  &> log/Sre-true-p3.log

# { echo "descartes Prop 1:"; time descartes -p=1 Time-false.java; } &> log/Time-false-p1.log
# { echo "descartes Prop 2:"; time descartes -p=2 Time-false.java; } &> log/Time-false-p2.log
# { echo "descartes Prop 3:"; time descartes -p=3 Time-false.java; } &> log/Time-false-p3.log
# { echo "descartes Prop 1:"; time descartes -p=1 Time-true.java; }  &> log/Time-true-p1.log
# { echo "descartes Prop 2:"; time descartes -p=2 Time-true.java; }  &> log/Time-true-p2.log
# { echo "descartes Prop 3:"; time descartes -p=3 Time-true.java; }  &> log/Time-true-p3.log

# { echo "descartes Prop 1:"; time descartes -p=1 TypeAdapter-false.java; } &> log/TypeAdapter-false-p1.log
# { echo "descartes Prop 2:"; time descartes -p=2 TypeAdapter-false.java; } &> log/TypeAdapter-false-p2.log
# { echo "descartes Prop 3:"; time descartes -p=3 TypeAdapter-false.java; } &> log/TypeAdapter-false-p3.log
# { echo "descartes Prop 1:"; time descartes -p=1 TypeAdapter-true.java; }  &> log/TypeAdapter-true-p1.log
# { echo "descartes Prop 2:"; time descartes -p=2 TypeAdapter-true.java; }  &> log/TypeAdapter-true-p2.log
# { echo "descartes Prop 3:"; time descartes -p=3 TypeAdapter-true.java; }  &> log/TypeAdapter-true-p3.log

# { echo "descartes Prop 1:"; time descartes -p=1 Value-false.java; } &> log/Value-false-p1.log
# { echo "descartes Prop 2:"; time descartes -p=2 Value-false.java; } &> log/Value-false-p2.log
# { echo "descartes Prop 3:"; time descartes -p=3 Value-false.java; } &> log/Value-false-p3.log
# { echo "descartes Prop 1:"; time descartes -p=1 Value-true.java; }  &> log/Value-true-p1.log
# { echo "descartes Prop 2:"; time descartes -p=2 Value-true.java; }  &> log/Value-true-p2.log
# { echo "descartes Prop 3:"; time descartes -p=3 Value-true.java; }  &> log/Value-true-p3.log
