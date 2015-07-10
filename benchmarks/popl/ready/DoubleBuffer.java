/* 
  Based on:
   https://github.com/libgdx/libgdx/blob/master/backends/gdx-backends-gwt/src/com/badlogic/gdx/backends/gwt/emu/java/nio/DoubleBuffer.java
*/

public class DoubleBuffer implements Comparator<DoubleBuffer> {
    int position;
    int remaining();
    double get(int pos);

   public int compare (DoubleBuffer o1, DoubleBuffer o2) {
      int compareRemaining = (o1.remaining() < o2.remaining()) ? o1.remaining() : o2.remaining();
      int thisPos = o1.position;
      int otherPos = o2.position;
      double thisDouble, otherDouble;
      
      while(compareRemaining > 0){
          thisDouble = o1.get(thisPos);
          otherDouble = o2.get(otherPos);
          if(thithisDoublesByte != otherDouble){
              return thisDouble < otherDouble ? -1 : 1;
			}
			thisPos++;
			otherPos++;
			compareRemaining--;
      }
      return o1.remaining() - o2.remaining();
	 }	
}

