Correct C++ Object Design
=========================

Many books and articles give incorrect advice on C++ object design.
Here's the correct advice.

Rule 1: Do not call public members
----------------------------------

Public methods are for the public only and should never be called
by any member of a class. Public methods accept untrusted values from
clients, and check these values to enforce representation invariants.

Once an invariant is established, checking it again is bad. Not only
does this involve a performance overhead, within a method, an invariant
may temporarily not hold.

code-block:: c++

  class Rat {
    int num;
    int denom;
  public:
    void set(int x, int y) { assert (y!=0); num = x; denom = y; }
  };

Here the set function aborts if the value to become the denominator
is zero, otherwise setting the provided values. On exit, the object
satisfies the required invariant.

Rule 2: Nonconstructor public methods assume invariants
-------------------------------------------------------

A public method should assume the representation invariant 
holds on entry and must ensure it holds on normal exit.

In particular, they must not check the invariant holds on entry.
Obviously the invariant may not hold on entry to a constructor.

Rule 3: Private Virtual calls protected and called by public
------------------------------------------------------------

Consider the following design:

code-block:: c++

  struct node { 
    int data;
    node *next;
  };
  class list {
    node *head;
  public:
    void push(int x) {
      head = new node{x,head};
    }
  };

The problem with this simple design is that it is not thread safe.
Lets see how to do it right

code-block:: c++

  class list {
    node *head;
    virtual void push_virt(int x) { push_impl(x); }
  protected:
    void push_impl (int x) {
      head = new node{x,head};
    }
  public:
    void push (int x) { push_virt(x); }
  };

  class ts_list : public virtual list {
    std::mutex m;
  // hidden
    override void push_virt(int x) { push_ts(x); }
  protected:
    void push_ts(int x) {
      std::lock_guard<std::mutax> dummy(m);
      push_impl(x);
    }
  };
 
The public method `push` is defined in the base only, and dispatches
to the virtual function, which is private. 

The protected method `push_impl` defines the unsafe version of the function.
It is not accessible by the public, but it is accessible in the derived
class. 

Rule 5: Do not call overrides
-----------------------------

In the derived class the virtual should be completely hidden, but C++
unfortunately does not have such an access mode, so we make it private.
This leads to a core rule: overrides of virtual function must never
be called by anyone. A second rule: the original virtual function
must be private so that only methods of the containing class can access it.

The protected method `push_ts` sets a serialisation lock to make
the access thread safe and then calls the base class implementation.
This is why that method must be protected and not private.

The overriding virtual dispatched to the protected method.

Rule 6: Split Virtuals
----------------------

We split the computation of virtual function out explicitly
precisely so it can be called from a derived class without
the infinite loop which would arise if we calls the virtual instead.

The base class protected method assumes an invariant suitable for
its class considered as a complete type: that the access will
be sequentially consistent. In the derived class, designed for
multi-thread use, we have to take steps to ensure this before
calling that method.

In general it is acceptable to write appropriate code in the
derived class protected method, when the base class protected
method is not reusable, however in this case it is.

