/*
 * Based on http://stackoverflow.com/questions/26322215/comparison-logic-error-with-economic-data-comparison-method-violates-its-gener
 * 
 */

public class DataPoint implements Comparator<DataPoint>{
  int fiscalQuarter;
  int sectorCode;
  int industryCode;

  public int compare(DataPoint o1, DataPoint o2) {
    int fiscalResult = Int.compare(o1.fiscalQuarter,o2.fiscalQuarter);
    if (fiscalResult > 0) {
        return fiscalResult;
    }
    if (fiscalResult < 0) {
        return fiscalResult;
    } 
    
    if (o1.sectorCode > 0) {
        if (o1.sectorCode > o2.sectorCode) {
            return o1.sectorCode - o2.sectorCode;
        }
        else {
          if (o1.sectorCode < o2.sectorCode){
            return o2.sectorCode - o1.sectorCode;
          } else {
            return 0; // Should never happen
          }
        }
    } else {
      if (o1.industryCode > 0) {
        if (o1.industryCode > o2.industryCode) {
            return o1.industryCode - o2.industryCode;
        }
        else {
          if (o1.industryCode < o2.industryCode) {
            return o2.industryCode - o1.industryCode;
          } else {
            return 0; // Should never happen
          }
        }
      }// These should never be reached
      else { 
        if (o1.sectorCode > 0) {
          return -1;
        } else {
          if (o2.industryCode() > 0) {
            return -1;
          } else {
            return 0;
          }
        }
      }
    }
  }
}