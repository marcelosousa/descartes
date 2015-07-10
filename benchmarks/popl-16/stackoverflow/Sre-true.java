/* 
 * Based on http://stackoverflow.com/questions/6626437/why-does-my-compare-method-throw-exception-comparison-method-violates-its-gen
 *
 */
public class SRE implements Comparator<SRE> {
    boolean getSponsored();
    
    public int compare(SRE o1, SRE o2) {
        return o1.getSponsored() == o2.getSponsored() ? 0 : o1.getSponsored() ? -1 : 1;
    }
}