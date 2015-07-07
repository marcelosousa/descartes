/*
 * Based on: http://stackoverflow.com/questions/9486605/comparison-method-violates-its-general-contract-in-java-7
 *
 */
public class Date implements Comparator<Date> {
		double calcSmoothDays();

   int compare (Date o1, Date o2) {
      double d1 = o1.calcSmoothDays();
      double d2 = o2.calcSmoothDays();

      if(d1 >= d2){
        return 1;
      }
      else {
        return -1;
      }
	 }
}
