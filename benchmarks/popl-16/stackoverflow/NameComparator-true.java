/*
 * Based on http://stackoverflow.xluat.com/questions/31235938/java-order-by-priority-list
 * 
 */

public class NameComparator implements Comparator<NameComparator>{
   int Name;
   
   public int compare(MyClass o1, MyClass o2){
     int x = o1.Name;
     int y = o2.Name;
     
     if(x == y){ return 0; }
     
     int i=0;     
     int currentName;

     while(i < 3){       
       currentName = nondet(i);
       if(currentName == x) {
         return 1;
       }
   
       if(currentName == y) {
         return -1;
       }
       i++;
     }
     
     return Int.compare(x,y);
   }
}

