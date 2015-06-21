public class Test01 implements Comparator<Test01> {
    int x; int y;
    public int compare(Test01 o1, Test01 o2) {
        if (o1.x < o2.x)
            return -1;
        else if (o1.x > o2.x)
            return 1;
        else if (o1.y < o2.y)
            return -1;
        else if (o1.y > o2.y)
            return 1;
        else
            return 0;
    }

}
