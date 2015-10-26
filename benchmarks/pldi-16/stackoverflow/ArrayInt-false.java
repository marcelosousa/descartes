/*
 * Based on http://stackoverflow.com/questions/23907134/comparing-two-arrays-using-dictionary-order-in-an-array-of-arrays-java
 * 
 */

public class AInt implements Comparator<AInt>{
   int length;
   int get(int pos);
   
   public int compare(AInt o1, AInt o2){
     int index, aentry, bentry;
     index = 0;
     
     while ((index < o1.length) && (index < o2.length)) {
       aentry = o1.get(index);
       bentry = o2.get(index);
       if (aentry < bentry) {
           return -1;
       }
       if (aentry > bentry) {
           return 1;
       }
       index++;
     }
     
     return 0;
   }
}

