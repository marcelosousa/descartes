/* 
 * Based on http://stackoverflow.com/questions/20970217/why-does-my-comparison-method-violate-its-general-contract
 *
 */
public class Container implements Comparator<Container>{
  boolean departureTimeIsBefore(int time);
  int departureTime;
  int departureMaxDuration;
  int departureTransportCompany;
  int departureTransportType;

  public int compare(Container o1, Container o2) {
      int rv;

      // Times
      rv = Int.compare(o1.departureTime, o2.departureTime);
      if (rv == 0) {
          // Duration
          if (o1.departureMaxDuration < o2.departureMaxDuration) {
              rv = -1;
          }
          else if (o1.departureMaxDuration > o2.departureMaxDuration) {
              rv = 1;
          }
          else {
              // Transport company
              rv = Int.compare(o1.departureTransportCompany, o2.departureTransportCompany);
              if (rv == 0) {
                  // Transport type
                  rv = Int.compare(o1.departureTransportType, o2.departureTransportType);
              }
          }
      }
      
      return rv;      
  }
}
