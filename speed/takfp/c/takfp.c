#include <stdio.h>
#include <stdlib.h>
#include <time.h>

float Tak (float x, float y, float z)
{
    if (y >= x) return z;
    return Tak(Tak(x-1.0f,y,z), Tak(y-1.0f,z,x), Tak(z-1.0f,x,y));
}

int main(int argc, char* argv[])
{
    int n = 10;
    clock_t t0 = clock();
    double r = Tak(n*3.0, n*2.0, n*1.0);
    clock_t t1 = clock();
    printf("%.2f\n", r);
    printf("%f\n", ((double) (t1 - t0)) / CLOCKS_PER_SEC);
    return 0;
}

