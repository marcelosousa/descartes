public class Crew implements Comparator<Crew>{
  int getRank();

  public int compare(Crew o1, Crew o2) {
      int myRank = o1.getRank();
      int hisRank = o2.getRank();
  
      if (o1 == o2) {
          return 0;
      }
      if (myRank > hisRank) {
          return 1;
      }
      if (myRank < hisRank) {
          return -1;
      }
      if (myRank == hisRank) {
  				return 0;
      }
      
      return 0;
  }

}
