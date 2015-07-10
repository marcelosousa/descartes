/* 
  Based on:
   https://github.com/libgdx/libgdx/blob/master/backends/gdx-backends-gwt/src/com/badlogic/gdx/backends/gwt/emu/java/nio/FloatBuffer.java
*/

public class FloatBuffer implements Comparator<FloatBuffer> {
    int position;
    int remaining();
    float get(int pos);

   public int compare (FloatBuffer o1, FloatBuffer o2) {
      int compareRemaining = (o1.remaining() < o2.remaining()) ? o1.remaining() : o2.remaining();
      int thisPos = o1.position;
      int otherPos = o2.position;
	  float thisFloat, otherFloat;
      
      while(compareRemaining > 0){
          thisFloat = o1.get(thisPos);
          otherFloat = o2.get(otherPos);
		  if ((thisFloat != otherFloat) && ((thisFloat == thisFloat) || (otherFloat == otherFloat))) {
		  	return thisFloat < otherFloat ? -1 : 1;
		  }
		  thisPos++;
		  otherPos++;
		  compareRemaining--;
      }
      return o1.remaining() - o2.remaining();
	 }
}

