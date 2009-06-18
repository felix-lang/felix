#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int Ack(int M, int N) {
  if (M==0) return N +1;
  else if(N==0) return Ack(M-1,1);
  else return Ack(M-1, Ack(M,N-1));
}

int main(int argc, char *argv[]) {
    int n = 13;
    clock_t t0 = clock();
    int v = Ack(3, n);
    clock_t t1 = clock();
    printf("Ack(3,%d): %d\n", n, v);
    printf("%f\n", ((double) (t1 - t0)) / CLOCKS_PER_SEC);
    return 0;
}

