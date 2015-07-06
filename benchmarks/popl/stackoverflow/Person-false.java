/*
 * Based on http://stackoverflow.com/questions/8327514/comparison-method-violates-its-general-contract
 *
 */
public class Person implements Comparator<Person>{
  Person getParent();

  int compare(Person o1, Person o2){
    if (o1.getParent() == o2) return -1;
    if (o2.getParent() == o1) return 1;
    return 0;
  }

}
