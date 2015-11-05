public class Obj implements Comparator<Obj>{
  int get(int i);

  public int compare(Obj o1, Obj o2){
    int i = 0;
  
    while(i < 1){
      if(o1.get(i) > o2.get(i)){return 1;}
     
      if(o1.get(i) < o2.get(i)){return -1;}
    
      i = i+1;
    }
  
    return 0;
  } 
}
/*
compare(Obj o1, Obj o2) {
  while (true) {
    if (i < 5) { break; }
    
    if (o1.get(i) > o2.get(i)) { break; }
    
    if (o1.get(i) < o2.get(i)) { break; }
    
    i++;
  }
  while (i < 5) {
    if(o1.get(i) > o2.get(i)) {
      break;
    }
    if(o1.get(i) < o2.get(i)) {
      break;
    }
    i++;
  }
  
  if
  
}

public (int,int,int) compare(Obj o1, Obj o2, Obj o3) {
  int i1, i2, i3 = 0;
  
  while (i1 < 5) {
    if (o1.get(i) > o2.get(i)) {
      while (i2 < 5) {
        if (o2.get(i) > o3.get(i)) {
          while (i3 < 5) {
            if (o1.get(i) > o3.get(i)) {
              return (1,1,1);
            }
          }
        }
      }
      
    }
  }
}
*/