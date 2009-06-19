import java.lang.System;

public class Test {
  public static void main(String[] args) {
    int n = 13;
    long t0 = java.lang.System.currentTimeMillis();
    int v = Ack(3, n);
    long t1 = java.lang.System.currentTimeMillis();
    System.out.println("Ack(3," + n + "): " + v);
    System.out.println((t1 - t0) / 1000.0);
  }
  public static int Ack(int m, int n) {
    return
      (m == 0) ? (n + 1) :
      ((n == 0) ? Ack(m-1, 1) : Ack(m-1, Ack(m, n - 1)))
    ;
  }
}
