public class Obj {
  int isString;
  int limit;
  int position;
  int get(int i);

  public boolean equals(Obj o1, Obj o2) {
    assume(o1.limit >= 0);
    assume(o1.limit == o2.limit);
    int o1rem = o1.limit - o1.position;
    int o2rem = o1.limit - o2.position;
    if (o1rem != o2rem) {
      return 0;
    }

    int i = o1.limit;
    while (i > o1.position) {
      if (o1.get(i) != o2.get(i)) {
        return 0;
      }
      i--;
    }
    return 1;
  }
}
