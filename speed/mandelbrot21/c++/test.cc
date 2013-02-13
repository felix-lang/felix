#include <iostream>

#define RESOLUTION 5000

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

    int    max_val = RESOLUTION/2;
    int    min_val = -max_val;
    double mul = 2.0 / max_val;
    int    count = 0;
    for(int i=min_val;i<=max_val;i++) {
        for(int j=min_val;j<=max_val;j++) {
            count += iters(100,mul*i,mul*j);
        }
    }

    std::cout << count << std::endl;
    return 0;
}
