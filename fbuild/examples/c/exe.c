#include <stdio.h>
#include "lib1.h"
#include "lib2.h"

int main(int argc, char** argv) {
  int x = fred1();
  int y = fred2();
  printf("%d %d\n", x, y);
  return x == 5 && y == 6 ? 0 : 1;
}
