/*
 * Based on http://stackoverflow.com/questions/23907134/comparing-two-arrays-using-dictionary-order-in-an-array-of-arrays-java
 * 
 */

public class StringCompare implements Comparator<StringCompare>{
   int length;
   int charAt(int pos);
   
   public int compare(StringCompare o1, StringCompare o2){
     int l1 = o1.length;
     int l2 = o2.length;
     int max = 0;
      if (l1 <= l2) {
          max = l1;
      }
      else
          max = l2;
          
      int count = 0;
     
      int i = 0;
      while (i < max){
                 int ch1 = str1.charAt(i);
                 int ch2 = str2.charAt(i);
     
                 if (str2.charAt(i) > str1.charAt(i)) {
                     return - 1;
                 }
     
                 if (str1.charAt(i) > str2.charAt(i)) {
                     return 1;
     
                 }
                 if (l1 == l2) {
                     if (ch1 == ch2) {
                         count++;
                     }
                     if (count == max) {
                         return 0;
                     }
                 }
     
             }
             if (l1 == l2) return 0;
             if (l1 > l2)
                 return 1;
             else
                 return - 1;
     
   }
}

