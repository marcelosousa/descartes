/*
 * Based on http://stackoverflow.com/questions/30479349/same-code-is-producing-different-outputs-for-java-7-8
 *
 */

public class ValueComparator implements Comparator<String> {
	 int get();

   int compare (String o1, String o2) {
     if(o1.get() > o2.get()){
       return -1;
     } else if(o1.get() < o2.get()) {
       return 1;
     } else {
       return o1.get() - o2.get();
     }
	 }
}
