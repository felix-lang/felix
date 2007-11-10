// strtable.h            see license.txt for copyright and terms of use
// implements a collection of immutable sm_strings with unique representatives

#ifndef STRTABLE_H
#define STRTABLE_H

#include "sm_strhash.h"

// fwd
class Flatten;

// global sm_string table for use during flattening/unflattening;
// it's up to clients to manage this ptr, as this module doesn't
// do anything besides define it into existence
// (this isn't the ideal solution..)
extern class StringTable *flattenStrTable;


// the type of references to sm_strings in a sm_string table; the pointer
// can be used directly in equality comparisons, because several calls
// to 'add' return the same pointer; and it points to the represented
// sm_string (null-terminated), so it can be printed directly, etc.
typedef char const *StringRef;


class StringTable {
private:    // types
  // constants
  enum {
    rackSize = 16000,      // size of one rack
    longThreshold = 1000,  // minimum length of a "long" sm_string
  };

  // some of the sm_strings stored in the table
  struct Rack {
    Rack *next;            // (owner) next rack, if any; for deallocation
    int usedBytes;         // # of bytes of 'data' that are used
    char data[rackSize];   // data where sm_strings are stored

  public:
    Rack(Rack *n) : next(n), usedBytes(0) {}
    int availBytes() const { return rackSize - usedBytes; }
    char *nextByte() { return data + usedBytes; }
  };

  // stores long sm_strings
  struct LongString {
    LongString *next;      // (owner) next long sm_string
    char *data;            // (owner) sm_string data, any length (null terminated)

  public:
    LongString(LongString *n, char *d) : next(n), data(d) {}
  };

private:    // data
  // hash table mapping sm_strings to pointers into one
  // of the sm_string racks
  StringHash hash;

  // linked list of racks; only walked at dealloc time; we add new
  // sm_strings to the first rack, and prepend a new one if necessary;
  // 'racks' is never null
  Rack *racks;

  // similar for long sm_strings
  LongString *longStrings;

private:    // funcs
  // not allowed
  StringTable(StringTable&);
  void operator=(StringTable&);
  void operator==(StringTable&);

  // for mapping data to keys, in the hashtable
  static char const *identity(void *data);

public:     // funcs
  StringTable();
  ~StringTable();

  // throw away everything in this table
  void clear();

  // add 'src' to the table, if it isn't already there; return a
  // unique representative, such that multiple calls to 'add' with
  // the same sm_string contents will always yield the same value
  StringRef add(char const *src);

  // some syntactic sugar
  StringRef operator() (char const *src) { return add(src); }

  // if 'src' is in the table, return its representative; if not,
  // return NULL
  StringRef get(char const *src) const;

  // similar functions for sm_strings with specified lengths
  // this doesn't work because the underlying hash table interface needs null terminators..
  //StringRef add(char const *src, int len);
  //StringRef get(char const *src, int len) const;

  // read/write binary
  void xfer(Flatten &flat, StringRef &ref);
};


#endif // STRTABLE_H

