#include <iostream>
#include <time.h>
static const int runs = 1;
static const int max_iterations = 99888;
long iterate(double ci, double cr)
{
    long count = 1;
    double zr = 0.0;
    double zi = 0.0;
    double zr2 = 0.0;
    double zi2 = 0.0;
    while((count <= max_iterations) && (zr2 + zi2 < 4.0))
    {
        zi = zr * zi * 2.0 + ci;
        zr = zr2 - zi2 + cr;
        ++count;
        zr2 = zr * zr;
        zi2 = zi * zi;
    }
    if(count > max_iterations)
    {
        return 0;
    }
    else
    {
        return count;
    }
}

void mandelbrot(long n)
{
    long i;
    for(int y = -39; y <= 38; ++y)
    {
        if(n == 1)
        {
            std::cout << '\n';
        }
        for(int x = -39; x <= 38; ++x)
        {
            i = iterate(x / 40.0, y / 40.0 -0.5);
            if(n == 1)
            {
                if(i == 0) std::cout << '*';
                else std::cout << ' ';
            }
        }
    }
}

int main()
{
    clock_t start, end;
    double cpu_time_used;
    start = clock();
    for(long iter = 1; iter<= runs; ++iter)
    {
        mandelbrot(iter);
    }
    end = clock();
    std::cout << std::endl;

    cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
    std::cout << cpu_time_used << std::endl;

    return 0;
}
