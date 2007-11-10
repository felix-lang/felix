// towner.cc            see license.txt for copyright and terms of use
// test owner stuff

#include "sm_owner.h"
#include <stdio.h>    // printf

class Foo;
void someCrap(Foo *f)
{
  //delete f;
  // good -- egcs-1.1.2 doesn't allow this, so I won't accidentally
  // get destructors missed because of forward decls (like could
  // happen with Borland)
}


// a simple class to play with
class Foo {
public:
  static int count;    // # of Foos there are
  int x;

public:
  Foo(int a);
  ~Foo();
};

int Foo::count = 0;

Foo::Foo(int ax)
  : x(ax)
{
  printf("created Foo at %p\n", this);
  count++;
}

Foo::~Foo()
{
  printf("destroying Foo at %p\n", this);
  count--;
}


void printFoo(Foo *f)
{
  printf("Foo at %p, x=%d\n", f, f? f->x : 0);
}

void printFooC(Foo const *f)
{
  printf("const Foo at %p, x=%d\n", f, f? f->x : 0);
}

void printInt(int x)
{
  printf("int x is %d\n", x);
}


// make it, forget to free it
void test1()
{
  printf("----------- test1 -----------\n");
  Owner<Foo> f;
  f = new Foo(4);
}

// access all of the operators as non-const
void test2()
{
  printf("----------- test2 -----------\n");
  Owner<Foo> f(new Foo(6));

  printFoo(f);
  (*f).x = 9;
  f->x = 12;
}

// access all of the operators as const
void test3()
{
  printf("----------- test3 -----------\n");
  Owner<Foo> f(new Foo(8));
  Owner<Foo> const &g = f;

  printFooC(g);
  printInt((*g).x);      // egcs-1.1.2 allows this for non-const operator fn!!!
  printInt(g->x);
}

// test exchange of ownership
void test4()
{
  printf("----------- test4 -----------\n");
  //Owner<Foo> f = new Foo(3);     // egcs-1.1.2 does the wrong thing here
  Owner<Foo> f(new Foo(3));
  Owner<Foo> g;
  g = f;
  printFoo(f);    // should be null
  f = g.xfr();
  printFoo(g);    // should be null
}


int main()
{
  test1();
  test2();
  test3();
  test4();

  printf("%d Foos leaked\n", Foo::count);
  return Foo::count;
}

