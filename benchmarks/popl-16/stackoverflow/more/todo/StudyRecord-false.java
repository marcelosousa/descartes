/*
 * Based on http://stackoverflow.com/questions/6402456/android-java-custom-sorting-using-comparable 
 * 
 */

public class StudyRecord implements Comparator<StudyRecord>{
    boolean isString;
    boolean isDate;
    boolean isInteger;
    boolean isUser;
    boolean isQuestionnaireStatusType;
    int getQuestionnaireStatusType;
    int toInt;

    @Override
    public int compare(StudyRecord o1, StudyRecord o2) {
       if (o1.isString && o2.isString) {
            return String.compareIgnoreCase(o1.toInt, o2.toInt); 
       }
        else if (o1.isQuestionnaireStatusType && o2.isQuestionnaireStatusType) {
            int status1 = o1.getQuestionnaireStatusType;
            int status2 = o2.getQuestionnaireStatusType;

            if(status1 == status2) {
              return 0;
            }

            if(status1 == 1) { 
              return -1;
            }

            if(status1 == 2) {
              if(status2 == 1) {
                  return 1;
              } else {
                  return -1;
              }
            }

            if(status1 == 3) {
              if((status2 == 1) || (status2 == 2)){
                return 1;
              } else {
                return -1;
              }
            }

            if(status1 == 4) {
              if(status2 == 5) {
                return -1;
              } else {
                return 11;
              }
            }

            if(status1 == 5) {
              return 1;
            }
        }
        else if (o1.isDate && o2.isDate) {
            return Int.compare(o1.toInt, o2.toInt); 
        } else if (o1.isInteger && o2.isInteger) {
            return Int.compare(o1.toInt, o2.toInt); 
        } else if (o1.isUser && o2.isUser) {
            return String.compareIgnoreCase(o1.toInt, o2.toInt);
        }

        return 0;
    }
}
