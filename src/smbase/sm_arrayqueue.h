// arrayqueue.h
// queue, implemented with an array

#ifndef ARRAYQUEUE_H
#define ARRAYQUEUE_H

#include "sm_xassert.h"

// needed operations on T:
//   T()                       // default ctor
//   operator=(T&)             // assignment
//   bool operator==(T&)       // comparison

template <class T>
class ArrayQueue {
private:     // data
  T *arr;                      // working storage
  int arrSize;                 // allocated length of 'arr'
  int head;                    // index of first element to dequeue
  int tail;                    // index+1 of last element to dequeue

  // NOTE: If head == tail then the queue is empty.  If head > tail,
  // then the queue elements circularly wrap around the end of 'arr'.
  // At all times, 0 <= head,tail < arrSize.

public:      // funcs
  ArrayQueue(int initSize = 10);
  ~ArrayQueue();

  // test # of elements in queue
  int length() const
    { return head<=tail? tail-head : arrSize-(head-tail); }
  bool isEmpty() const                  { return head==tail; }
  bool isNotEmpty() const               { return !isEmpty(); }

  // add/remove elements in FIFO order
  void enqueue(T const &t);
  T dequeue();

  // remove all elements
  void empty()                          { head = tail = 0; }

  // access elements of the queue in dequeue order; that is,
  // element 0 is the next element to be dequeued, and element
  // length()-1 is the element most recently enqueued
  //
  // as this interface is O(1), it is the intended method
  // of iterating over the elements in the queue
  T const &eltC(int index) const;
  T &elt(int index)                     { return const_cast<T&>(eltC(index)); }
  T &operator[] (int index)             { return elt(index); }
  T const &operator[] (int index) const { return eltC(index); }

  // reverse the sequence of stored elements
  void reverse();

  // true if a specific element is among the queue elements
  bool contains(T const &t) const;
};


template <class T>
ArrayQueue<T>::ArrayQueue(int initSize)
{
  // initial size must be positive, since array growth is
  // simply by doubling the size
  xassert(initSize > 0);

  arr = new T[initSize];
  arrSize = initSize;
  head = tail = 0;
}


template <class T>
ArrayQueue<T>::~ArrayQueue()
{
  delete[] arr;
}


template <class T>
void ArrayQueue<T>::enqueue(T const &t)
{
  if (length() == arrSize-1) {
    // must expand the queue

    // make new array
    int newArrSize = arrSize * 2;
    T *newArr = new T[newArrSize];

    // copy elements sequentially
    int oldLength = length();
    for (int i=0; i<oldLength; i++) {
      newArr[i] = eltC(i);
    }

    // discard old array
    delete[] arr;

    // put new one in its place
    arr = newArr;
    arrSize = newArrSize;
    head = 0;
    tail = oldLength;
  }

  // store the new element where 'tail' points
  arr[tail] = t;

  // advance 'tail'
  if (++tail == arrSize) {
    tail = 0;
  }
}


template <class T>
T ArrayQueue<T>::dequeue()
{
  if (isEmpty()) {
    xfailure("attempt to dequeue an empty queue");
  }

  // advance 'head' while yielding the element it currently points at;
  // avoid making an intermediate copy (for performance)
  if (head == arrSize-1) {
    head = 0;
    return arr[arrSize-1];
  }
  else {
    return arr[head++];
  }
}


template <class T>
T const &ArrayQueue<T>::eltC(int index) const
{
  xassert(0 <= index && index < length());

  if (head+index < arrSize) {
    return arr[head+index];
  }
  else {
    return arr[head+index - arrSize];
  }
}


template <class T>
void ArrayQueue<T>::reverse()
{
  int i = 0, j = length()-1;
  while (i < j) {
    // swap i,j elements
    T tmp = elt(i);
    elt(i) = elt(j);
    elt(j) = tmp;

    i++;
    j--;
  }
}


template <class T>
bool ArrayQueue<T>::contains(T const &t) const
{
  int len=length();
  for (int i=0; i<len; i++) {
    if (t == eltC(i)) {
      return true;
    }
  }
  return false;
}


#endif // ARRAYQUEUE_H
