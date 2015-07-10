public class CharBuffer implements Comparator<CharBuffer> {
    int position;
    int remaining();
    int get(int pos);

   public int compare (CharBuffer o1, CharBuffer o2) {
      int minRemaining = (o1.remaining() < o2.remaining()) ? o1.remaining() : o2.remaining();
      int thisPos = o1.position;
      int otherPos = o2.position;
      int thisByte, otherByte;

      int i = 0;      
      while(i < minRemaining){
          thisByte = o1.get(thisPos + i);
          otherByte = o2.get(otherPos + i);
          if(thisByte != otherByte){
              return thisByte < otherByte ? -1 : 1;
		  		}
		  		i++;
      }
      
      return o1.remaining() - o2.remaining();
	 }
}
