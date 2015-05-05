public class test04 {
    public static void main(String[] args) {
        test04 f = new test04();
        int a = 7;
        int b = 14;
        int x = (f.bar(21) + a) * b;
    }
    public int bar(int n) { return n + 42; }
    int x; int y; int z;
    public int compare(test04 o1, test04 o2) {
        if (o1.x < o2.x)
            return -1;
        if (o1.x > o2.x)
            return 1;
        if (o1.y < o2.y)
            return -1;
        test04 o3=o2;
        if (o1.y > o3.y)
            return 1;
        if (o1.z < o3.z)
            return -1;
        if (o1.z > o3.z)
            return 1;
        return 0;
    }

}
