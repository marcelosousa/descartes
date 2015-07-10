/*
 * Based on http://codedbot.com/questions/5112702/comparison-method-violates-its-general-contractarraylist-comprasion 
 * 
 */

public class Word implements Comparator<Word>{
    int count;
    int get(int i);
    int length;

    @Override
    public int compare(Word o1, Word o2) {
      int left = o1.count;
      int right = o2.count;

      if (left == right) {
        if(o1.length > o2.length){
          int i1 = 0;
          while (i1 < o1.length){
            if(Int.compare(o1.get(i1), o2.get(i1)) < 0)
              return 1;
            i1++; 
          }
          return -1;
        }
        else {
          int i2 = 0;
          while(i2 < o2.length){
            if(Int.compare(o1.get(i2), o2.get(i2)) < 0)
              return -1;
            i2++;    
          }
          return 1;
        }
      }
      else return (left > right)? 1:-1;
    }
}
