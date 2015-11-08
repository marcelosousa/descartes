public class Obj {
  int isString;
  int isWrongString;
  int variable;

  public int equals(Obj o1, Obj o2){
    if (o2.isString == 1) {
      return equals(o1.variable, o2.variable);
    }

    if (o2.isWrongString == 1) {
      return equals(o1.variable, o2.variable);
    }

    return 0;
  } 
}
