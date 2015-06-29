public class PokerHand implements Comparator<String> {
  /* http://stackoverflow.com/questions/30449488/comparison-method-violates-its-general-contract-everything-seems-ok */
	public int compare(String c1, String c2) {
        if (c1.indexOf('4') != -1 || c2.indexOf('4') != -1) {  // Four of a kind
            if (c1.indexOf('4') == c2.indexOf('4')) {
                for (int i = 12; i >= 0; i--) {
                    if (c1.charAt(i) != '0' && c1.charAt(i) != '4') {
                        if (c2.charAt(i) != '0' && c2.charAt(i) != '4') {
                            return 0;
                        }
                        return 1;
                    }
                    if (c2.charAt(i) != '0' && c2.charAt(i) != '4') {
                        return -1;
                    }
                }
            }
            return c1.indexOf('4') - c2.indexOf('4');
        }
        int tripleCount1 = StringFunctions.countOccurrencesOf(c1, "3");
        int tripleCount2 = StringFunctions.countOccurrencesOf(c2, "3");
        if (tripleCount1 > 1 || (tripleCount1 == 1 && c1.indexOf('2') != -1) || tripleCount2 > 1 || (tripleCount2 == 1 && c2.indexOf('2') != -1)) {  // Full house
            int higherTriple = c1.lastIndexOf('3');
            if (higherTriple == c2.lastIndexOf('3')) {
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
