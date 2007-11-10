// sm_stringset.h            see license.txt for copyright and terms of use
// set of character sm_strings

#ifndef STRINGSET_H
#define STRINGSET_H

#include "sm_strsobjdict.h"

class StringSet {
private:     // data
  // represent using a dictionary of pointers to nothing
  StringSObjDict<int> elts;

public:      // funcs
  StringSet() : elts() {}
  ~StringSet();

  // # elts in the set
  int size() const                        { return elts.size(); }

  bool isEmpty() const                    { return elts.isEmpty(); }
  bool isNotEmpty() const                 { return elts.isNotEmpty(); }

  // true if elt is in the set
  bool contains(char const *elt) const    { return elts.isMapped(elt); }

  // add elt to the set; ok if it's already there
  void add(char const *elt);

  // remove elt from the set; ok if it's not there now
  void remove(char const *elt);

  // empty the set
  void empty()                            { elts.empty(); }
};

#endif // STRINGSET_H
