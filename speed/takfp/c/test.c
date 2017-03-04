#include <stdio.h>
#include <stdlib.h>

double Tak (double x, double y, double z)
{
    if (y >= x) return z;
    return Tak(Tak(x-1.0,y,z), Tak(y-1.0,z,x), Tak(z-1.0,x,y));
}

int main(int argc, char* argv[])
{
    int n = 12;
    double r = Tak(n*3.0, n*2.0, n*1.0);
    printf("%.2f\n", r);
    return 0;
}

