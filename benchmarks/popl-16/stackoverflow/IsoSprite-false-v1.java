/*
 * Based on http://stackoverflow.xluat.com/questions/30824837/using-comparable-to-compare-different-variables
 *
 */

public class IsoSprite implements Comparator<IsoSprite> {
  int minX;
  int maxX;
  int minY;
  int maxY;
  int minZ;
  int maxZ;
   
  public int compare(IsoSprite o1, IsoSprite o2){
    if ((o2.maxX > o1.minX) && (o2.maxY > o1.minY) && (o2.minZ < o1.maxZ)){
            return -1;
    }else if((o2.maxX < o1.minX) && (o2.maxY < o1.minY) && (o2.minZ > o1.maxZ)){
            return 1;
    }

    return 0;
  }

}
