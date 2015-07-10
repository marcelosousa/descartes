/* 
  Based on:
   https://github.com/libgdx/libgdx/blob/master/backends/gdx-backends-gwt/src/com/badlogic/gdx/backends/gwt/emu/java/nio/CharBuffer.java
*/

public class CharBuffer implements Comparator<CharBuffer> {
    int position;
    int remaining();
    char get(int pos);

   public int compare (CharBuffer o1, CharBuffer o2) {
      int compareRemaining = (o1.remaining() < o2.remaining()) ? o1.remaining() : o2.remaining();
      int thisPos = o1.position;
      int otherPos = o2.position;
      char thisByte, otherByte;
      
      while(compareRemaining > 0){
          thisByte = o1.get(thisPos);
          otherByte = o2.get(otherPos);
          if(thisByte != otherByte){
              return thisByte < otherByte ? -1 : 1;
			}
			thisPos++;
			otherPos++;
			compareRemaining--;
      }
      return o1.remaining() - o2.remaining();
	 }
}
