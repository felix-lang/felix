#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>

float Tak (float x, float y, float z)
{
    if (y >= x) return z;
    return Tak(Tak(x-1,y,z), Tak(y-1,z,x), Tak(z-1,x,y));
}

int main(int argc, char* argv[])
{
    /* int n = ((argc == 2) ? atoi(argv[1]) : 1); */
    clock_t start, end;
    double cpu_time_used;
    int n = 10;
    start = clock();
    double r = Tak(n*3.0, n*2.0, n*1.0);
    //printf("%.1f\n", Tak(n*3.0, n*2.0, n*1.0));
    end = clock();
    cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
    printf("%f\n",cpu_time_used);
    return 0;
}

