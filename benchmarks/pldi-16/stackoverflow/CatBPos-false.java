/* 
 * Based on http://stackoverflow.com/questions/22768302/why-i-get-comparison-method-violates-its-general-contract
 *
 */
public class CatBPos implements Comparator<CatBPos>{
  int getHdopBPosGetTime;
  int getBPosGetTime;
  int getBPosGetStat;
  boolean getBPosIsDeparture;
  boolean getBPosIsVacation;
  boolean getBPosIsDeparture;
  boolean getBPosIsArrival;
  boolean getHdopBPosGetTimeIsNotVoid;

  public int compare(CatBPos o1, CatBPos o2) {
    int lCompare;

    if (o1.getHdopBPosGetTimeIsNotVoid && o2.getHdopBPosGetTimeIsNotVoid) {
        lCompare = Int.compare(o1.getHdopBPosGetTime, o2.getHdopBPosGetTime);
        if (lCompare != 0) {
            return lCompare;
        }
    }

    lCompare = Int.compare(o1.getBPosGetTime, o2.getBPosGetTime);
    if (lCompare != 0) {
        return lCompare;
    }

    if (o1.getBPosIsDeparture && o2.getBPosIsVacation) {
        return 1;
    } else if (o1.getBPosIsVacation && o2.getBPosIsArrival) {
        return 1;
    }

    // Ankunft und Abfahrt für denselben Bahnhof sollen in der richtigen Reihenfolge sein
    if (o1.getBPosIsDeparture && o2.getBPosIsArrival && (o1.getBPosGetStat == o2.getBPosGetStat)) {
        return 1;
    }

    return 0;
  }
}
