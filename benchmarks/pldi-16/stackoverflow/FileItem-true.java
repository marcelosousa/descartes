/*
 * Based on http://stackoverflow.com/questions/30458633/why-does-my-compare-methd-throw-illegalargumentexception-sometimes
 *
 */

public class FileItem implements Comparator<FileItem> {
	 int getFileName();
   int toInt();

   int compare (FileItem o1, FileItem o2) {
				int result = 0;
        if (o1.toInt() == null){
          if (o2.toInt() == null){
            return 0;
          } else {
            return 1;
          }
        } else if (o2.toInt() == null) {
          return -1;
        }
 
        int n1 = o1.getFileName();
        int n2 = o2.getFileName();
        
        if (n1 == null) {
          if (n2 == null) {
            return 0;
          } else {
            return 1;
          }
        } else if (n2 == null) {
          return -1;
        }
        return n1 - n2;
	 }
}
