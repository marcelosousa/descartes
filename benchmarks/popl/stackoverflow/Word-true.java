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
 
      if (left == right){
        int i = 0;
        while ((i < o1.length) && (i < o2.length)){
          if((o1.get(i) - o2.get(i)) < 0)
            return -1;

          if((o1.get(i) - o2.get(i)) > 0)
            return 1;

          i++;
        }

        return o1.length - o2.length;
      } 
      else return (left > right)? 1:-1;
   }
}
