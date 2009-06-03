// based off http://metamatix.org/~ocaml/price-of-abstraction.html

#include <sys/time.h>
#include <iostream>

#define RESOLUTION 5000

void set_fpu (unsigned int mode) {
    asm ("fldcw %0" : : "m" (*&mode));
}

class Complex {
public:
   Complex(double xc,double yc) : x(xc), y(yc) { }

   double norm_square() {
      return x*x+y*y;
   }

   Complex operator*(const Complex &other) {
      return Complex(x*other.x-y*other.y,
                     x*other.y+y*other.x);
   }

   Complex operator+(const Complex &other) {
      return Complex(x + other.x, y + other.y);
   }

private:
   double x;
   double y;
};

int iters(int max_iter,double xc,double yc) {
   Complex c(xc,yc);
   Complex z(xc,yc);
   for(int count = 0;count<max_iter;count++) {
      if( z.norm_square() >= 4.0 ) { return count; }
      z = z * z + c;
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
