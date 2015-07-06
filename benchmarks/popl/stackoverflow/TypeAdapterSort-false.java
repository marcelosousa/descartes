/*
 * Based on http://stackoverflow.com/questions/16075779/comparison-method-throws-general-contract-exception
 *
 */
public class TypeAdapterSort implements Comparator<TypeAdapterSort> {
   int order;

   int compare (TypeAdapterSort o1, TypeAdapterSort o2) {
      if (o1.order < o2.order){
        return -1;
      } else {
        return 1;
      }
   }
}
