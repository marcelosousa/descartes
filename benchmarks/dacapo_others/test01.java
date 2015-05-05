public class test01 {
    public static void main(String[] args) {
        test01 f = new test01();
        int a = 7;
        int b = 14;
        int x = (f.bar(21) + a) * b;
    }
    public int bar(int n) { return n + 42; }
    int x; int y;
    public int compare(test01 o1, test01 o2) {
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
