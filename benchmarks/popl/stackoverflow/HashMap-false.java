/*
 * Based on http://stackoverflow.com/questions/19325256/java-lang-illegalargumentexception-comparison-method-violates-its-general-contr
 *
 */

public class Node implements Comparator<Node> {
  int getID();
  boolean containsKey(int id);
  int get(int id);
   
  public int compare(Node o1, Node o2){
    if(o1.containsKey(o1.getID()) && o2.containsKey(o2.getID())){
      int order1 = o1.get(o1.getID());
      int order2 = o2.get(o2.getID());

      if(order1 < order2)
        return -1;
      else if(order1 > order2)
        return 1;
      else
        return 0;
    }

    return 0;
  }

}
