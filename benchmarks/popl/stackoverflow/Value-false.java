/*
 * Based on http://stackoverflow.com/questions/8327514/comparison-method-violates-its-general-contract
 *   and on http://stackoverflow.com/questions/23788633/comparison-method-violates-its-general-contract-how-to-make-it-transitive
 *
 */
public class Value implements Comparator<Value> {
  int value;

  int compare(Value o1, Value o2){
    if (o1.value < o2.value)
      return -1;
    else if (o1.value >= o2.value)
      return 1;
    else
      return 0;    
  }
}
