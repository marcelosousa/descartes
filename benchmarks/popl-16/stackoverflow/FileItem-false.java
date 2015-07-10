/* 
 * Based on http://stackoverflow.com/questions/30458633/why-does-my-compare-methd-throw-illegalargumentexception-sometimes
 *
 */

public class FileItem implements Comparator<FileItem> {
	 int getFileName();
   int toInt();

   int compare (FileItem o1, FileItem o2) {
				int result = 0;
        if ((o1.toInt() != null) && (o2.toInt() != null)) {

            int n1 = o1.getFileName();
            int n2 = o2.getFileName();

            if ((n1 != null) && (n2 != null))
                result = n1 - n2; //n1.compareTo(n2);
        }

        return result;
	 }
}
