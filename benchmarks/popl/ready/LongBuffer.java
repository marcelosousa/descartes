/* 
  Based on:
   https://github.com/libgdx/libgdx/blob/master/backends/gdx-backends-gwt/src/com/badlogic/gdx/backends/gwt/emu/java/nio/IntBuffer.java
*/

public class LongBuffer implements Comparator<LongBuffer> {
    int position;
    int remaining();
    long get(int pos);

   public int compare (IntBuffer o1, IntBuffer o2) {
      int compareRemaining = (o1.remaining() < o2.remaining()) ? o1.remaining() : o2.remaining();
      int thisPos = o1.position;
      int otherPos = o2.position;
	  long thisLong, otherLong;
      
      while(compareRemaining > 0){
          thisLong = o1.get(thisPos);
          otherLong = o2.get(otherPos);
		  if (thisLong != otherLong) {
		  	return thisLong < otherLong ? -1 : 1;
		  }
		  thisPos++;
		  otherPos++;
		  compareRemaining--;

      }
      return o1.remaining() - o2.remaining();
	 }
}
