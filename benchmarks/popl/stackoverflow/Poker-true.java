public class PokerHand implements Comparator<PokerHand> {
  /* http://stackoverflow.com/questions/30449488/comparison-method-violates-its-general-contract-everything-seems-ok */

  int indexOf(int pos);
  int charAt(int i);
  int countOccurrencesOf(int i);
  int lastIndexOf(int i);
 
	public int compare(PokerHand o1, PokerHand o2) {
        int i=12;
        if ((o1.indexOf(4) != -1) || (o2.indexOf(4) != -1)) {  // Four of a kind
            if (o1.indexOf(4) == o2.indexOf(4)) {
                while(i >= 0){
                    if ((o1.charAt(i) != 0) && (o1.charAt(i) != 4) && (o2.charAt(i) != 0) && (o2.charAt(i) != 4)) {
                    		return 0;
                    }

                    if ((o1.charAt(i) != 0) && (o1.charAt(i) != 4)) {
                        return 1;
                    }
									  
                    if ((o2.charAt(i) != 0) && (o2.charAt(i) != 4)) {
                        return -1;
                    }
										i--;
                }
            }
            return o1.indexOf(4) - o2.indexOf(4);
        }
        int tripleCount1 = o1.countOccurrencesOf(3);
        int tripleCount2 = o2.countOccurrencesOf(3);
        if ((tripleCount1 > 1) || ((tripleCount1 == 1) && (o1.indexOf(2) != -1))) {  // Full house
						if ((tripleCount2 > 1) || ((tripleCount2 == 1) && (o2.indexOf(2) != -1))) {
            	int higherTriple = o1.lastIndexOf(3);
            	if (higherTriple == o2.lastIndexOf(3)) {
                i = 12;
                while(i >= 0) {
                    if ((i != higherTriple) && (o1.charAt(i) == 2) || (o1.charAt(i) == 3) && (o2.charAt(i) == 2) || (o2.charAt(i) == 3)) {
                    	     return 0;
                    	}
                    if ((i != higherTriple) && (o1.charAt(i) == 2) || (o1.charAt(i) == 3)) {
                    	    return 1;
                    }
                    if ((i != higherTriple) && (o2.charAt(i) == 2) || (o2.charAt(i) == 3)) {
                    	    return -1;
                    }
                    i--;
                }
							}
            	return higherTriple - o2.lastIndexOf(3);
        }
        return 1;
    }
    if ((tripleCount2 > 1) || ((tripleCount2 == 1) && (o2.indexOf(2) != -1))) {
            return -1;
        }
        return 0;
    }
}
