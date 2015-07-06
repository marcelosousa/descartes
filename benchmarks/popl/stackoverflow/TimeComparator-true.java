/*
 * Based on http://stackoverflow.com/questions/10234038/compare-method-throw-exception-comparison-method-violates-its-general-contract
 *
 */
public class Time implements Comparator<Time> {
   int ora;
   int volume_totale;

   int compare (Time o1, Time o2) {
      int time1 = o1.ora;
      int time2 = o2.ora;

			int cmp = Int.compare(time1, time2);
      if (cmp == 0){
        int voltot1 = o1.volume_totale;
        int voltot2 = o2.volume_totale;
        
        cmp = Int.compare(voltot1, voltot2);
      }
      return cmp; 
	 }
}
