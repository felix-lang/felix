import java.lang.System;

public class Test {
  public static void main(String args[]) {
    int n = 10;
    long t0 = java.lang.System.currentTimeMillis();
    float v = Tak(n*3.0f, n*2.0f, n*1.0f);
    long t1 = java.lang.System.currentTimeMillis();
    System.out.format("%.2f\n", v);
    System.out.println((t1 - t0) / 1000.0);
  }

  public static float Tak (float x, float y, float z) {
    if (y >= x) return z;
    else return Tak(Tak(x-1.0f,y,z), Tak(y-1.0f,z,x), Tak(z-1.0f,x,y));
  }
}

