public class ack {
  public static void main(String[] args) {
    int num = Integer.parseInt(args[0]);
    System.out.println("Ack(3," + num + "): " + Ack(3, num));
  }
  public static int Ack(int m, int n) {
    return
      (m == 0) ? (n + 1) :
      ((n == 0) ? Ack(m-1, 1) : Ack(m-1, Ack(m, n - 1)))
    ;
  }
}

