/*
 * Based on http://sohu.io/questions/2211707/comparison-method-violates-its-general-contract
 *
 */

public class NzbFile implements Comparator<NzbFile> {
  int getFileName();
  boolean getFileName_toLowerCase_endsWith(int str);
  int getSubject();
   
  int compare(NzbFile o1, NzbFile o2){
    if ((o1.getFileName() != null) && (o2.getFileName() != null)){
        if (o1.getFileName_toLowerCase_endsWith(1)) // ".nfo"
            return -1000;
        else if (o2.getFileName_toLowerCase_endsWith(1)) // ".nfo"
            return 1000;
        else if (o1.getFileName_toLowerCase_endsWith(2)) // ".sfv"
            return -999;
        else if (o2.getFileName_toLowerCase_endsWith(2)) // ".sfv"
            return 1001;
        else if (o1.getFileName_toLowerCase_endsWith(3)) // ".srr"
            return -998;
        else if (o2.getFileName_toLowerCase_endsWith(3)) // ".srr"
            return 1002;
        else if (o1.getFileName_toLowerCase_endsWith(4)) // ".nzb"
            return -997;
        else if (o2.getFileName_toLowerCase_endsWith(4)) // ".nzb"
            return 1003;
        else if (o1.getFileName_toLowerCase_endsWith(5)) //".srt"
            return -996;
        else if (o2.getFileName_toLowerCase_endsWith(5)) // ".srt"
            return 1004;
        else
            return Int.compare(o1.getFileName(), o2.getFileName());
    }
    else if ((o1.getFileName() != null) && (o2.getFileName() == null))
    {
        return -995;
    }
    else if ((o1.getFileName() == null) && (o2.getFileName() != null))
    {
        return 1005;
    }
    else
    {
        return Int.compare(o1.getSubject(), o2.getSubject());
    }
  }

} 
