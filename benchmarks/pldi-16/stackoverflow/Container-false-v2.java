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
      if (o1.departureTimeIsBefore(o2.departureTime))
          return -1;
      else if (o1.departureTimeIsBefore(o2.departureTime))
          return 1;
      else {
          if ((o1.departureMaxDuration == o2.departureMaxDuration) && 
              (o1.departureTransportType == o2.departureTransportType) && 
              (o1.departureTransportCompany == o1.departureTransportCompany))
                return 0;
          else
                return 1;
      }
  }
}
