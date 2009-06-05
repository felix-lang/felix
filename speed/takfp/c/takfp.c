#include <stdio.h>
#include <stdlib.h>

float Tak (float x, float y, float z)
{
    if (y >= x) return z;
    return Tak(Tak(x-1,y,z), Tak(y-1,z,x), Tak(z-1,x,y));
}

int main(int argc, char* argv[])
{
    int n = ((argc == 2) ? atoi(argv[1]) : 1);
    printf("%.1f\n", Tak(n*3.0, n*2.0, n*1.0));
    return 0;
}

