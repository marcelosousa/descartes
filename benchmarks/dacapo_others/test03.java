public class test03 {
    public static void main(String[] args) {
        test03 f = new test03();
        int a = 7;
        int b = 14;
        int x = (f.bar(21) + a) * b;
    }
    public int bar(int n) { return n + 42; }
    int x; int y; int z;
    public int compare(test03 o1, test03 o2) {
        if (o1.x < o2.x)
            return -1;
        else if (o1.x > o2.x)
            return 1;
        else if (o1.y < o2.y)
            return -1;
        else if (o1.y > o2.y)
            return 1;
        else if (o1.z < o2.z)
            return -1;
        else if (o1.z > o2.z)
            return 1;
        else
            return 0;
    }

}
