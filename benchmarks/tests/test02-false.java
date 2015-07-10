public class MyInt implements Comparator<MyInt> {
    int x;

    /* Breaks anti-symmetry */
    int compare (MyInt o1, MyInt o2) {
      if (o1.x >= o2.x)
			  return 1;
			else 
			  return -1;
	 }
}
