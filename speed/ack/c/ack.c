#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>

int Ack(int M, int N) {
  if (M==0) return N +1;
  else if(N==0) return Ack(M-1,1);
  else return Ack(M-1, Ack(M,N-1));
}

int main(int argc, char *argv[]) {
    clock_t start, end;
    double cpu_time_used;
    start = clock();
    int n = 13;
    int j = Ack(3,n);
    end = clock();
    cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
    printf("%f\n",cpu_time_used);
    /*
    int n = atoi(argv[1]);
    printf("Ack(3,%d): %d\n", n, Ack(3, n));
    */
    return(0);
}

