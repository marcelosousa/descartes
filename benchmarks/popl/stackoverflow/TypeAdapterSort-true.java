/*
 * Based on http://stackoverflow.com/questions/16075779/comparison-method-throws-general-contract-exception
 *
 */

public class TypeAdapterSort implements Comparator<TypeAdapterSort> {
   int order;

   int compare (TypeAdapterSort o1, TypeAdapterSort o2) {
     return o1.order == o2.order ? 0
       : o1.order < o2.order ? -1
       : 1;
   }
}
