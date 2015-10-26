/*
 * Based on http://sohu.io/questions/2211707/comparison-method-violates-its-general-contract
 * Not working yet!
 */

public class NzbFile implements Comparator<NzbFile> {
  int getFileName;
  boolean getFileName_toLowerCase_endsWith(int str);
  int getSubject();
   
  int compare(NzbFile o1, NzbFile o2){
    if (o1 == o2) { return 0; }
    if ((o1.getFileName != null) && (o2.getFileName != null)){
        int i = 0;
        while (i < 5){
          if(o1.getFileName_toLowerCase_endsWith(i) && o2.getFileName_toLowerCase_endsWith(i)){
            return 0;
          }
           if(o1.getFileName_toLowerCase_endsWith(i)){
            return -1000 + i;
          } 
          if(o2.getFileName_toLowerCase_endsWith(i)){
            return 1000 + i;
          }
          i++;
        }
        return Int.compare(o1.getFileName, o2.getFileName);
    }
    else if ((o1.getFileName != null) && (o2.getFileName == null))
    {
        return -995;
    }
    else if ((o1.getFileName == null) && (o2.getFileName != null))
    {
        return 1005;
    }
    else
    {
        return Int.compare(o1.getSubject(), o2.getSubject());
    }
  }

} 
