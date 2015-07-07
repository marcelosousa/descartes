public class PokerHand implements Comparator<PokerHand> {
  /* http://stackoverflow.com/questions/30449488/comparison-method-violates-its-general-contract-everything-seems-ok */

  int indexOf(int pos);
  int charAt(int i);
  int countOccurrencesOf(int i);
  int lastIndexOf(int i);
 
  public int compare(PokerHand o1, PokerHand o2) {
    int i=0;
    if ((o1.indexOf(4) != -1) || (o2.indexOf(4) != -1)) {  // Four of a kind
      // One the hands has a foud of a kind
      if (o1.indexOf(4) == o2.indexOf(4)) {
        while(i <= 12 && (!((o1.charAt(i) != 0) && (o1.charAt(i) != 4))) && (!((o2.charAt(i) != 0) && (o2.charAt(i) != 4)))){
          i++;
        }
          if ((o1.charAt(i) != 0) && (o1.charAt(i) != 4)) {
              if ((o2.charAt(i) != 0) && (o2.charAt(i) != 4)) {
                  return 0;
              }
              return 1;
          }
          if ((o2.charAt(i) != 0) && (o2.charAt(i) != 4)) {
              return -1;
          }
      }
      return o1.indexOf(4) - o2.indexOf(4);
    }
    
    int tripleCount1 = o1.countOccurrencesOf(3);
    int tripleCount2 = o2.countOccurrencesOf(3);
    if ((tripleCount1 > 1) || ((tripleCount1 == 1) && (o1.indexOf(2) != -1)) || (tripleCount2 > 1) || ((tripleCount2 == 1) && (o2.indexOf(2) != -1))) {  // Full house
        int higherTriple = o1.lastIndexOf(3);
        if (higherTriple == o2.lastIndexOf(3)) {
            i=0;
            while((i <= 12) && (i == higherTriple) )
            for (int i = 12; i >= 0; i--) {
                if (i == higherTriple) {
                    continue;
                }
                if (c1.charAt(i) == '2' || c1.charAt(i) == '3') {
                    if (c2.charAt(i) == '2' || c2.charAt(i) == '3') {
                        return 0;
                    }
                    return 1;
                }
                if (c2.charAt(i) == '2' || c2.charAt(i) == '3') {
                    return -1;
                }
            }
        }
        return higherTriple - c2.lastIndexOf('3');
    }
    return 0;
  }
}
