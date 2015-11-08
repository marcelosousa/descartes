public class Obj {
  int isString;
  int valueLength;
  int get(int i);

  public boolean equals(Obj o1, Obj o2) {
    assume(o1.isString == 1); 
    if (o2.isString == 1) {
      int n = o1.valueLength;
      if (n == o2.valueLength) {
          int i = 0;
          while (i < n) {
              if (o1.get(i) != o2.get(i)) {
                      return 1;
              }
              i++;
          }
          return 1;
      }
    }
    return 0;
  }
}
