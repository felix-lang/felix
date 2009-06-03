// based off http://metamatix.org/~ocaml/price-of-abstraction.html

#include <sys/time.h>
#include <iostream>

#define RESOLUTION 5000

void set_fpu (unsigned int mode) {
    asm ("fldcw %0" : : "m" (*&mode));
}

int iters(int max_iter, double xc, double yc) {
    double x = xc;
    double y = yc;
    for(int count = 0;count < max_iter; count++) {
        if( x*x + y*y >= 4.0) { return count; }
        double tmp = x*x-y*y+xc;
        y = 2.0 * x * y + yc;
        x = tmp;
    }
    return max_iter;
}

int main() {
    set_fpu (0x27F);

    timeval t0;
    if (gettimeofday(&t0, NULL)) {
        std::cerr << "gettimeofday failed" << std::endl;
        return 1;
    }

    int    max_val = RESOLUTION/2;
    int    min_val = -max_val;
    double mul = 2.0 / max_val;
    int    count = 0;
    for(int i=min_val;i<=max_val;i++) {
        for(int j=min_val;j<=max_val;j++) {
            count += iters(100,mul*i,mul*j);
        }
    }

    timeval t1;
    if (gettimeofday(&t1, NULL)) {
        std::cerr << "gettimeofday failed" << std::endl;
        return 1;
    }

    std::cout << count << std::endl;

    double t = t1.tv_sec - t0.tv_sec;
    t += double(t1.tv_usec - t0.tv_usec) / 1000000.0;
    std::cout << t << std::endl;

    return 0;
}
