public class PairInt implements Comparator<PairInt> {
    int x;
    int y;

    /* Breaks anti-symmetry */
    int compare (PairInt o1, PairInt o2) { 
      if (o1.x < o2.x)
			  return -1;
			else if (o1.y < o2.y)
			  return -1;
			else if (o1.x > o2.x)
			  return 1;
			else if (o1.y > o2.y)
			  return 1; 
      else
			  return 0;
	 }
}
