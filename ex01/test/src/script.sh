PROJECT_DIR=/home/mihir/git/overload_checker/ex01
LD_LIBRARY_PATH=/home/mihir/lib java -cp $PROJECT_DIR/lib/com.microsoft.z3.jar:$PROJECT_DIR/lib/soot.jar:$PROJECT_DIR/bin edu.utexas.cs.OverloadMain -pp -cp . test01 -f J
