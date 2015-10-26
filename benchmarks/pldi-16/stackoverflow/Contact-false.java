/* 
 * Based on http://stackoverflow.com/questions/26336978/comparison-method-violates-its-general-contract-and-method-compareto
 *
 */
public class Contact implements Comparator<Contact>{
  int getFirstName;
  int getLastName;
  int getEmails;

  public int compare(Contact o1, Contact o2) {
      int compareFirstName = 0;
      if ((o1.getFirstName != 0) && (o2.getFirstName != 0)) {
        compareFirstName = String.compareIgnoreCase(o1.getFirstName, o2.getFirstName); 

        if (compareFirstName == 0) {
            int compareLastName = 0;
            if ((o1.getLastName != 0) && (o2.getLastName != 0)) {
                compareLastName = String.compareIgnoreCase(o1.getLastName, o2.getLastName);

                if (compareLastName == 0) {
                    int compareEmail = 0;
                    if ((o1.getEmails != 0) && (o2.getEmails != 0)) {
                        compareEmail = String.compareIgnoreCase(o1.getEmails, o2.getEmails);
                        return compareEmail;
                    } else {
                        return 0;
                    }
                } else {
                    return compareLastName;
                }
            } else {
                int compareEmail = 0;
                if ((o1.getEmails != 0) && (o2.getEmails != 0)) {
                    compareEmail = String.compareIgnoreCase(o1.getEmails, o2.getEmails);
                    return compareEmail;
                } else {
                    return 0;
                }
            }
        } else {
            return compareFirstName;
        }
    } else {
        int compareLastName = 0;
        if ((o1.getLastName != 0) && (o2.getLastName != 0)) {
            compareLastName = String.compareIgnoreCase(o1.getLastName, o2.getLastName);

            if (compareLastName == 0) {
                int compareEmail = 0;
                if ((o1.getEmails != 0) && (o2.getEmails != 0)) {
                    compareEmail = String.compareIgnoreCase(o1.getEmails, o2.getEmails);
                    return compareEmail;
                } else {
                    return 0;
                }
            } else {
                return compareLastName;
            }
        } else {
            int compareEmail = 0;
            if ((o1.getEmails != 0) && (o2.getEmails != 0)) {
                compareEmail = String.compareIgnoreCase(o1.getEmails, o2.getEmails);
                return compareEmail;
            } else {
                return 0;
            }
        }
    }
  }
}