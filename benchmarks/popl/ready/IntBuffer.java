/* 
  Based on:
   https://github.com/libgdx/libgdx/blob/master/backends/gdx-backends-gwt/src/com/badlogic/gdx/backends/gwt/emu/java/nio/IntBuffer.java
*/

public class IntBuffer implements Comparator<IntBuffer> {
    int position;
    int remaining();
    int get(int pos);

   public int compare (IntBuffer o1, IntBuffer o2) {
      int compareRemaining = (o1.remaining() < o2.remaining()) ? o1.remaining() : o2.remaining();
      int thisPos = o1.position;
      int otherPos = o2.position;
	  int thisInt, otherInt;
      
      while(compareRemaining > 0){
          thisInt = o1.get(thisPos);
          otherInt = o2.get(otherPos);
		  if (thisInt != otherInt) {
		  	return thisInt < otherInt ? -1 : 1;
		  }
		  thisPos++;
		  otherPos++;
		  compareRemaining--;

      }
      return o1.remaining() - o2.remaining();
	 }
}