/*
 * Based on http://stackoverflow.com/questions/8327514/comparison-method-violates-its-general-contract
 *
 */

public class SimpleString implements Comparator<SimpleString>{
  int length();
  int toInt;

  public int compare(SimpleString o1, SimpleString o2){
    if (o1.length() == 0){
      if (o2.length() == 0){
        return 0;
      }
      return 1;
    }
   
    if (o2.length() == 0){
      return -1;
    }

    return String.compareIgnoreCase(o1.toInt, o2.toInt);
  }
}
