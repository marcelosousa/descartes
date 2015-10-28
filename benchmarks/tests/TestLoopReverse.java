public class Obj implements Comparator<Obj>{
  int get(int i);

  public int compare(Obj o1, Obj o2){
    int i = 5;
  
    while(i > 0){
      if(o1.get(i) > o2.get(i)){return 1;}
     
      if(o1.get(i) < o2.get(i)){return -1;}
    
      i = i-1;
    }
  
    return 0;
  } 
}
