public class Test implements Comparator<Test> {
  int X;
  int Y;
  public int compare(Test o1, Test o2){
    int y = 0;
    if (o1.X >= 0){
      if (o1.Y >= 0){
         y = 1;
      }else{
         y = -1;
      }
    } 
    else{
      if (o1.Y >= 0){
         y = 2;
      }else{
         y = -2;
      }
    }
    return y;
  }
}
