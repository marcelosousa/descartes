/* 
 * Based on http://dertompson.com/2012/11/23/sort-algorithm-changes-in-java-7/
 *
 */
public class ExportTerm implements Comparator<ExportTerm>{
  int sortLabel;

  public int compare(ExportTerm o1, ExportTerm o2) {
    if (o1.sortLabel == null){
      return -1;
    }
        if (o2.sortLabel == null){
      return 1;
    }

    return Int.compare(o1.sortLabel, o2.sortLabel);
  }

}
