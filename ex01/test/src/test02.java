public class test02 implements java.lang.Comparable<test02> {
    public static void main(String[] args) {
        test02 f = new test02();
        int a = 7;
        int b = 14;
        int x = (f.bar(21) + a) * b;
    }
    public int bar(int n) { return n + 42; }
    int x; int y;
	@Override
	public int compareTo(test02 o) {
        if (this.x < o.x)
            return -1;
        else if (this.x > o.x)
            return 1;
        if (this.y < o.y)
            return -1;
        else if (this.y > o.y)
            return 1;
        else
            return 0;
    }
}
