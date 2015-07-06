/*
 * Based on http://stackoverflow.com/questions/11441666/java-error-comparison-method-violates-its-general-contract
 *
 */

public class CollectionItem implements Comparator<CollectionItem> {
	 int getCardSet();
   int getCardRarity();
   int getCardId();
   int cardType;


   int compare (CollectionItem o1, CollectionItem o2) {
     if (o1 == o2){
       return 0;
     }
		 if (o1.getCardSet() > o2.getCardSet()) {
		     return 1;
		 }
		 if (o1.getCardSet() < o2.getCardSet()) {
		     return -1;
		 }
		 if (o1.getCardRarity() < o2.getCardRarity()) {
		     return 1;
		 }
		 if (o1.getCardRarity() > o2.getCardRarity()) {
		     return -1;
		 }
		 if (o1.getCardId() > o2.getCardId()) {
		     return 1;
		 }
		 if (o1.getCardId() < o2.getCardId()) {
		     return -1;
		 }
		 return o1.cardType - o2.cardType;  //watch out for overflow!

	 }

}
