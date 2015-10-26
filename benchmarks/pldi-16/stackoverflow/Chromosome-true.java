/*
 * Based on http://stackoverflow.com/questions/16809000/how-to-make-the-compareto-method-respect-the-general-contract
 * 
 */

public class Chromosome implements Comparator<Chromosome>{
  int getScore(int num);
  int isNull;

  public int compare(Chromosome o1, Chromosome o2) {
      assume(o1.isNull != 0);
      if(o2.isNull == 0)
          return(1);
      int comp = 0;
      int i = 0;
      while(i < 5){
          comp = Double.compare(o1.getScore(i), o2.getScore(i));
          if (comp!=0)
           return comp;            
          i++;
      }

      return 0;
  }
}
