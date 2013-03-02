#include <stdio.h>
#include <stdlib.h>

int Ack(int M, int N) {
  if (M==0) return N +1;
  else if(N==0) return Ack(M-1,1);
  else return Ack(M-1, Ack(M,N-1));
}

int main(int argc, char *argv[]) {
    int n = 13;
    int v = Ack(3, n);
    printf("Ack(3,%d): %d\n", n, v);
    return 0;
}

