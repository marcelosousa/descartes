public class MyInt implements Comparator<MyInt>, Comparable<MyInt> {
//    int val;
    int x;
    int y;

    // Precondition: Transitivity
/*    public int compare(MyInt o1, MyInt o2) {
     return o1.val - o2.val;
     // return 1;
    }
*/
/*
    int compare (MyInt o1, MyInt o2) { 
      if (o1.x < o2.x)
			  return -1;
			else if (o1.x > o2.x)
			  return 1;
			else if (o1.y < o2.y)
			  return -1;
			else if (o1.y > o2.y)
			  return 1; 
      else
			  return 0;
	 }
*/
   int compare (MyInt o1, MyInt o2) {
      int a = 1;
      a += 1;

      return a; 
   /*   if (o1.x < o2.x)
			  return 1;
      else
        return 1;
   */
	 }
}
