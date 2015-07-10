/*
 * Based on http://codedbot.com/questions/5138521/comparison-method-violates-its-general-contract-overlapping-conditions 
 * 
 */

public class Match implements Comparator<Match>{
    int score;
    int seq1start;
    int seq2start;

    @Override
    public int compare(Match o1, Match o2) {
    if(o1.score > o2.score)
      return -1;
    if((o1.score == o2.score) && ((o1.seq1start < o2.seq1start) && (o1.seq2start < o2.seq2start))) 
        return -1;
    if((o1.score == o2.score) && !((o1.seq1start < o2.seq1start) && (o1.seq2start < o2.seq2start)))
        return 0;
    return 1;
    }
}
