// parsetables.cc            see license.txt for copyright and terms of use
// code for parsetables.h

#include "elk_parsetables.h"
#include "sm_bflatten.h"
#include "sm_trace.h"
#include "sm_crc.h"
#include "elk_emitcode.h"
#include "sm_bit2d.h"

#include <string.h>         // memset
#include <stdlib.h>         // qsort, system


// array index code
enum { UNASSIGNED = -1 };


// fwd
template <class EltType>
void printTable(EltType const *table, int size, int rowLength,
                char const *typeName, char const *tableName);


ParseTables::ParseTables(int t, int nt, int s, int p, StateId start, int final)
{
  alloc(t, nt, s, p, start, final);
}

template <class T>
void allocInitArray(T *&arr, int size, T init)
{
  arr = new T[size];
  for (int i=0; i<size; i++) {
    arr[i] = init;
  }
}

template <class T>
void allocZeroArray(T *&arr, int size)
{
  arr = new T[size];
  memset(arr, 0, sizeof(arr[0]) * size);
}

void ParseTables::alloc(int t, int nt, int s, int p, StateId start, int final)
{
  owning = true;

  temp = new TempData(s);

  numTerms = t;
  numNonterms = nt;
  numStates = s;
  numProds = p;

  actionCols = numTerms;
  actionRows = numStates;

  gotoCols = numNonterms;
  gotoRows = numStates;

  allocZeroArray(actionTable, actionTableSize());

  allocZeroArray(gotoTable, gotoTableSize());

  allocZeroArray(prodInfo, numProds);

  allocZeroArray(stateSymbol, numStates);

  // table of ambiguous actions is NULL until someone fills in the
  // whole thing; since we don't know how many there might be, we
  // can't even allocate the storage now
  ambigTableSize = 0;
  ambigTable = NULL;

  startState = start;
  finalProductionIndex = final;

  allocZeroArray(nontermOrder, nontermOrderSize());

  if (ENABLE_CRS_COMPRESSION) {
    allocZeroArray(firstWithTerminal, numTerms);
    allocZeroArray(firstWithNonterminal, numNonterms);
  }
  else {
    firstWithTerminal = NULL;
    firstWithNonterminal = NULL;
  }

  bigProductionListSize = 0;
  bigProductionList = NULL;
  if (ENABLE_CRS_COMPRESSION) {
    allocZeroArray(productionsForState, numStates);
  }
  else {
    productionsForState = NULL;
  }

  if (ENABLE_CRS_COMPRESSION) {
    allocZeroArray(ambigStateTable, numStates);
  }
  else {
    ambigStateTable = NULL;
  }

  // # of bytes, but rounded up to nearest 32-bit boundary
  errorBitsRowSize = ((numTerms+31) >> 5) * 4;

  // no compressed info
  uniqueErrorRows = 0;
  errorBits = NULL;
  errorBitsPointers = NULL;

  actionIndexMap = NULL;
  actionRowPointers = NULL;

  gotoIndexMap = NULL;
  gotoRowPointers = NULL;
}


ParseTables::~ParseTables()
{
  if (temp) {
    delete temp;
  }

  if (owning) {
    delete[] actionTable;
    delete[] gotoTable;
    delete[] prodInfo;
    delete[] stateSymbol;

    if (ambigTable) {
      delete[] ambigTable;
    }

    delete[] nontermOrder;

    if (firstWithTerminal) {
      delete[] firstWithTerminal;
    }
    if (firstWithNonterminal) {
      delete[] firstWithNonterminal;
    }

    if (bigProductionList) {
      delete[] bigProductionList;
    }

    if (errorBits) {
      delete[] errorBits;
    }
    if (actionIndexMap) {
      delete[] actionIndexMap;
    }
    if (gotoIndexMap) {
      delete[] gotoIndexMap;
    }
  }

  // these are always owned
  if (productionsForState) {
    delete[] productionsForState;
  }
  if (ambigStateTable) {
    delete[] ambigStateTable;
  }
  if (errorBitsPointers) {
    delete[] errorBitsPointers;
  }
  if (actionRowPointers) {
    delete[] actionRowPointers;
  }
  if (gotoRowPointers) {
    delete[] gotoRowPointers;
  }
}


ParseTables::TempData::TempData(int numStates)
  : ambigTable(),
    bigProductionList(),
    productionsForState(numStates),
    ambigStateTable(numStates)
{
  productionsForState.setAll(UNASSIGNED);
  ambigStateTable.setAll(UNASSIGNED);
}

ParseTables::TempData::~TempData()
{}


ActionEntry ParseTables::validateAction(int code) const
{
  // make sure that 'code' is representable; if this fails, most likely
  // there are more than 32k states or productions; in turn, the most
  // likely cause of *that* would be the grammar is being generated
  // automatically from some other specification; you can change the
  // typedefs of ActionEntry and GotoEntry in gramanl.h to get more
  // capacity
  ActionEntry ret = (ActionEntry)code;
  xassert((int)ret == code);
  return ret;
}

GotoEntry ParseTables::validateGoto(int code) const
{
  // see above
  GotoEntry ret = (GotoEntry)code;
  xassert((int)ret == code);
  xassert(ret != errorGotoEntry);    // otherwise collision with error code
  return ret;
}


// doesn't init anything; for use by emitConstructionCode's emitted code
ParseTables::ParseTables(bool o)
  : owning(o),
    temp(NULL)
{
  xassert(owning == false);
}


#if ENABLE_CRS_COMPRESSION
ActionEntry makeAE(ActionEntryKind k, int index)
{
  // must fit into 6 bits for my encoding
  if ((unsigned)index <= AE_MAXINDEX) {
    // ok
  }
  else {
    // this is just so I can see the resulting truncated table;
    // the parser will *not* work
    std::cout << "error: index " << index << " truncated!\n";
    index = AE_MAXINDEX;
  }

  if (k == AE_ERROR) {
    xassert(index == 0);
  }

  return k | index;
}
#endif


ActionEntry ParseTables::encodeShift(StateId destState, int shiftedTermId)
{
  #if ENABLE_CRS_COMPRESSION
    int delta = destState - firstWithTerminal[shiftedTermId];
    return makeAE(AE_SHIFT, delta);
  #else
    return validateAction(+destState+1);
  #endif
}


ActionEntry ParseTables::encodeReduce(int prodId, StateId inWhatState)
{
  #if ENABLE_CRS_COMPRESSION
    int begin = temp->productionsForState[inWhatState];
    int end = temp->bigProductionList.length();
    if (begin == UNASSIGNED) {
      // starting a new set of per-state productions
      temp->productionsForState[inWhatState] = end;
      temp->bigProductionList.push(prodId);
      return AE_REDUCE | 0 /*first in set*/;
    }
    else {
      // continuing a set; search for existing 'prodId' in that set
      int delta;
      for (int i=begin; i<end; i++) {
        if (temp->bigProductionList[i] == prodId) {
          // re-use this offset
          delta = i-begin;
          goto encode;
        }
      }

      // not found: add another production id to this set
      temp->bigProductionList.push(prodId);
      delta = end-begin;

    encode:
      return makeAE(AE_REDUCE, delta);
    }

  #else
    return validateAction(-prodId-1);
  #endif
}


ActionEntry ParseTables::encodeAmbig
  (ArrayStack<ActionEntry> const &set, StateId inWhatState)
{
  #if ENABLE_CRS_COMPRESSION
    int begin = temp->ambigStateTable[inWhatState];
    int end = temp->ambigTable.length();
    if (begin == UNASSIGNED) {
      // starting a new set of per-state ambiguous actions
      temp->ambigStateTable[inWhatState] = end;
      appendAmbig(set);
      return makeAE(AE_AMBIGUOUS, 0 /*first in set*/);
    }
    else {
      // continuing a set: Look for another ambiguous action set in
      // the same line that has identical contents.  Due to the way
      // sets are constructed, their representation is canonical.
      // This is important because some grammars (cc2) have many
      // ambiguous entries, but they're all the same set of actions;
      // were we to not consolidate like this, the 6-bit cell encoding
      // would not be enough.

      // # of big-table entries that will be used
      int encodeLen = set.length()+1;

      for (int i=begin; i+encodeLen <= end; i++) {
        // does this offset contain the same set of actions?
        if (compareAmbig(set, i)) {
          return makeAE(AE_AMBIGUOUS, i-begin /*delta*/);
        }
      }

      // no match
      appendAmbig(set);
      return makeAE(AE_AMBIGUOUS, end-begin /*delta*/);
    }

  #else
    int end = temp->ambigTable.length();
    appendAmbig(set);
    return validateAction(numStates+end+1);
  #endif
}


void ParseTables::appendAmbig(ArrayStack<ActionEntry> const &set)
{
  temp->ambigTable.push(set.length());
  for (int j=0; j < set.length(); j++) {
    temp->ambigTable.push(set[j]);
  }
}

bool ParseTables::compareAmbig(ArrayStack<ActionEntry> const &set,
                               int startIndex)
{
  if (temp->ambigTable[startIndex] != set.length()) {
    return false;           // mismatch in 1st entry
  }
  for (int j=0; j < set.length(); j++) {
    if (temp->ambigTable[startIndex+1+j] != set[j]) {
      return false;         // mismatch in j+2nd entry
    }
  }
  return true;              // match!
}


ActionEntry ParseTables::encodeError() const
{
  #if ENABLE_CRS_COMPRESSION
    return makeAE(AE_ERROR, 0);
  #else
    return validateAction(0);
  #endif
}


GotoEntry ParseTables::encodeGoto(StateId destState, int shiftedNontermId) const
{
  #if ENABLE_CRS_COMPRESSION
    xassert(0 <= shiftedNontermId && shiftedNontermId < numNonterms);
    int delta = destState - firstWithNonterminal[shiftedNontermId];
    return validateGoto(delta);
  #else
    return validateGoto(destState);
  #endif
}


// simple alloc + copy
template <class T>
void copyArray(int &len, T *&dest, ArrayStack<T> const &src)
{
  len = src.length();
  dest = new T[len];
  memcpy(dest, src.getArray(), sizeof(T) * len);
}

// given an array 'src' of indices relative to 'base', allocate the
// array 'dest' and fill it in with actual pointers into 'base'
template <class T>
void copyIndexPtrArray(int len, T **&dest, T *base, ArrayStack<int> const &src)
{
  dest = new T* [len];
  for (int i=0; i<len; i++) {
    if (src[i] != UNASSIGNED) {
      dest[i] = base + src[i];
    }
    else {
      dest[i] = NULL;      // so segfault if deref unassigned entry
    }
  }
}

void ParseTables::finishTables()
{
  // copy the ambiguous actions
  copyArray(ambigTableSize, ambigTable, temp->ambigTable);

  if (ENABLE_CRS_COMPRESSION) {
    // transfer bigProductionList
    copyArray(bigProductionListSize, bigProductionList, temp->bigProductionList);

    // transfer productionsForState, translating indices into pointers
    copyIndexPtrArray(numStates, productionsForState, bigProductionList,
                      temp->productionsForState);

    // ambigStateTable
    copyIndexPtrArray(numStates, ambigStateTable, ambigTable,
                      temp->ambigStateTable);
  }

  delete temp;
  temp = NULL;
}


// -------------------- table compression --------------------
void ParseTables::computeErrorBits()
{
  traceProgress() << "computing errorBits[]\n";

  // should only be done once
  xassert(!errorBits);

  // allocate and clear it
  int rowSize = ((numTerms+31) >> 5) * 4;
  allocZeroArray(errorBits, numStates * rowSize);

  // build the pointer table
  allocZeroArray(errorBitsPointers, numStates);

  // find and set the error bits
  fillInErrorBits(true /*setPointers*/);

  // compute which rows are identical; I only compress the rows (and
  // not the columns) because I can fold the former's compression into
  // the errorBitsPointers[] access, whereas the latter would require
  // yet another table
  int *compressed = new int[numStates];   // row -> new location in errorBits[]
  uniqueErrorRows = 0;
  int s;
  for (s=0; s < numStates; s++) {
    // is 's' the same as any rows that preceded it?
    for (int t=0; t < s; t++) {
      // do 's' and 't' have the same contents?
      if (0==memcmp(errorBitsPointers[s],
                    errorBitsPointers[t],
                    sizeof(ErrorBitsEntry) * errorBitsRowSize)) {
        // yes, map 's' to 't' instead
        compressed[s] = compressed[t];
        goto next_s;
      }
    }

    // not the same as any
    compressed[s] = uniqueErrorRows;
    uniqueErrorRows++;

  next_s:
    ;
  }

  // make a smaller 'errorBits' array
  delete[] errorBits;
  allocZeroArray(errorBits, uniqueErrorRows * rowSize);

  // rebuild 'errorBitsPointers' according to 'compressed'
  for (s=0; s < numStates; s++) {
    errorBitsPointers[s] = errorBits + (compressed[s] * errorBitsRowSize);
  }
  delete[] compressed;

  // fill in the bits again, using the new pointers map
  fillInErrorBits(false /*setPointers*/);
}


void ParseTables::fillInErrorBits(bool setPointers)
{
  for (int s=0; s < numStates; s++) {
    if (setPointers) {
      errorBitsPointers[s] = errorBits + (s * errorBitsRowSize);
    }

    for (int t=0; t < numTerms; t++) {
      if (isErrorAction(actionEntry((StateId)s, t))) {
        ErrorBitsEntry &b = errorBitsPointers[s][t >> 3];
        b |= 1 << (t & 7);
      }
    }
  }
}


void ParseTables::mergeActionColumns()
{
  traceProgress() << "merging action columns\n";

  // can only do this if we've already pulled out the errors
  xassert(errorBits);

  // for now I assume we don't have a map yet
  xassert(!actionIndexMap);

  if (tracingSys("mergeActionColumnsPre")) {
    // print the action table before compression
    printTable(actionTable, actionTableSize(), actionCols,
               "ActionEntry", "actionTable");
  }

  // compute graph of conflicting 'action' columns
  // (will be symmetric)
  Bit2d graph(point(numTerms, numTerms));
  graph.setall(0);

  // fill it in
  for (int t1=0; t1 < numTerms; t1++) {
    for (int t2=0; t2 < t1; t2++) {
      // does column 't1' conflict with column 't2'?
      for (int s=0; s < numStates; s++) {
        ActionEntry a1 = actionEntry((StateId)s, t1);
        ActionEntry a2 = actionEntry((StateId)s, t2);

        if (isErrorAction(a1) ||
            isErrorAction(a2) ||
            a1 == a2) {
          // no problem
        }
        else {
          // conflict!
          graph.set(point(t1, t2));
          graph.set(point(t2, t1));
          break;
        }
      }
    }
  }

  // color the graph
  Array<int> color(numTerms);      // terminal -> color
  int numColors = colorTheGraph(color, graph);

  // build a new, compressed action table; the entries are initialized
  // to 'error', meaning every cell starts as don't-care
  ActionEntry *newTable;
  allocInitArray(newTable, numStates * numColors, errorActionEntry);

  // merge columns in 'actionTable' into those in 'newTable'
  // according to the 'color' map
  actionIndexMap = new TermIndex[numTerms];
  for (int t=0; t<numTerms; t++) {
    int c = color[t];

    // merge actionTable[t] into newTable[c]
    for (int s=0; s<numStates; s++) {
      ActionEntry &dest = newTable[s*numColors + c];

      ActionEntry src = actionEntry((StateId)s, t);
      if (!isErrorAction(src)) {
        // make sure there's no conflict (otherwise the graph
        // coloring algorithm screwed up)
        xassert(isErrorAction(dest) ||
                dest == src);

        // merge the entry
        dest = src;
      }
    }

    // fill in the action index map
    TermIndex ti = (TermIndex)c;
    xassert(ti == c);     // otherwise value truncation happened
    actionIndexMap[t] = ti;
  }

  trace("compression")
    << "action table: from " << (actionTableSize() * sizeof(ActionEntry))
    << " down to " << (numStates * numColors * sizeof(ActionEntry))
    << " bytes\n";

  // replace the existing table with the compressed one
  delete[] actionTable;
  actionTable = newTable;
  actionCols = numColors;
}


// unsurprisingly, this function has considerable structure in common
// with 'mergeActionColumns'; however, my attempts to consolidate them
// have led to code that is harder to understand and debug, so they
// remain separate (at least for now)
void ParseTables::mergeActionRows()
{
  traceProgress() << "merging action rows\n";

  // can only do this if we've already pulled out the errors
  xassert(errorBits);

  // for now I assume we don't have a map yet
  xassert(!actionRowPointers);

  // compute graph of conflicting 'action' rows
  // (will be symmetric)
  Bit2d graph(point(numStates, numStates));
  graph.setall(0);

  // fill it in
  for (int s1=0; s1 < numStates; s1++) {
    for (int s2=0; s2 < s1; s2++) {
      // does row 's1' conflict with row 's2'?
      for (int t=0; t < actionCols; t++) {    // t is an equivalence class of terminals
        ActionEntry a1 = actionTable[s1*actionCols + t];
        ActionEntry a2 = actionTable[s2*actionCols + t];

        if (isErrorAction(a1) ||
            isErrorAction(a2) ||
            a1 == a2) {
          // no problem
        }
        else {
          // conflict!
          graph.set(point(s1, s2));
          graph.set(point(s2, s1));
          break;
        }
      }
    }
  }

  // color the graph
  Array<int> color(numStates);      // state -> color (equivalence class)
  int numColors = colorTheGraph(color, graph);

  // build a new, compressed action table
  ActionEntry *newTable;
  allocInitArray(newTable, numColors * actionCols, errorActionEntry);

  // merge rows in 'actionTable' into those in 'newTable'
  // according to the 'color' map

  // actionTable[]:
  //
  //             t0    t1    t2    t3      // terminal equivalence classes
  //   s0
  //   s1
  //   s2
  //    ...
  //   /*states*/

  // newTable[]:
  //
  //             t0    t1    t2    t3      // terminal equivalence classes
  //   c0
  //   c1
  //   c2    < e.g., union of state1 and state4 (color[1]==color[4]==2) >
  //    ...
  //   /*state equivalence classes (colors)*/

  actionRowPointers = new ActionEntry* [numStates];
  for (int s=0; s<numStates; s++) {
    int c = color[s];

    // merge actionTable row 's' into newTable row 'c'
    for (int t=0; t<actionCols; t++) {
      ActionEntry &dest = newTable[c*actionCols + t];

      ActionEntry src = actionTable[s*actionCols + t];
      if (!isErrorAction(src)) {
        // make sure there's no conflict (otherwise the graph
        // coloring algorithm screwed up)
        xassert(isErrorAction(dest) ||
                dest == src);

        // merge the entry
        dest = src;
      }
    }

    // fill in the row pointer map
    actionRowPointers[s] = newTable + c*actionCols;
  }

  trace("compression")
    << "action table: from " << (numStates * actionCols * sizeof(ActionEntry))
    << " down to " << (numColors * actionCols * sizeof(ActionEntry))
    << " bytes\n";

  // replace the existing table with the compressed one
  delete[] actionTable;
  actionTable = newTable;
  actionRows = numColors;

  // how many single-value rows?  I'm investigating some other options
  // for further compression...
  {
    int ct=0;
    for (int s=0; s<actionRows; s++) {
      int val = 0;
      for (int t=0; t<actionCols; t++) {
        int entry = actionRowPointers[s][t];
        if (val==0) {
          val = entry;
        }
        else if (entry != 0 && entry != val) {
          // not all the same
          goto next_s;
        }
      }

      // all same
      ct++;

    next_s:
      ;
    }
    trace("compression") << ct << " same-valued action rows\n";
  }
}


// created by copying 'mergeGotoRows' and replacing 'action'
// with 'goto', etc.
void ParseTables::mergeGotoColumns()
{
  traceProgress() << "merging goto columns\n";

  // can only do this if we've already pulled out the errors
  xassert(errorBits);

  // for now I assume we don't have a map yet
  xassert(!gotoIndexMap);

  // compute graph of conflicting 'goto' columns
  Bit2d graph(point(numNonterms, numNonterms));
  graph.setall(0);

  // fill it in
  for (int nt1=0; nt1 < numNonterms; nt1++) {
    for (int nt2=0; nt2 < nt1; nt2++) {
      // does column 't1' conflict with column 't2'?
      for (int s=0; s < numStates; s++) {
        GotoEntry g1 = gotoEntry((StateId)s, nt1);
        GotoEntry g2 = gotoEntry((StateId)s, nt2);

        if (isErrorGoto(g1) ||
            isErrorGoto(g2) ||
            g1 == g2) {
          // no problem
        }
        else {
          // conflict!
          graph.set(point(nt1, nt2));
          graph.set(point(nt2, nt1));
          break;
        }
      }
    }
  }

  // color the graph
  Array<int> color(numNonterms);      // nonterminal -> color
  int numColors = colorTheGraph(color, graph);

  // build a new, compressed goto table; the entries are initialized
  // to 'error', meaning every cell starts as don't-care
  GotoEntry *newTable;
  allocInitArray(newTable, numStates * numColors, encodeGotoError());

  // merge columns in 'gotoTable' into those in 'newTable'
  // according to the 'color' map
  gotoIndexMap = new NtIndex[numNonterms];
  for (int nt=0; nt<numNonterms; nt++) {
    int c = color[nt];

    // merge gotoTable[nt] into newTable[c]
    for (int s=0; s<numStates; s++) {
      GotoEntry &dest = newTable[s*numColors + c];

      GotoEntry src = gotoEntry((StateId)s, nt);
      if (!isErrorGoto(src)) {
        // make sure there's no conflict (otherwise the graph
        // coloring and/or conflict map algorithms screwed up)
        xassert(isErrorGoto(dest) ||
                dest == src);

        // merge the entry
        dest = src;
      }
    }

    // fill in the goto index map
    NtIndex nti = (NtIndex)c;
    xassert(nti == c);     // otherwise value truncation happened
    gotoIndexMap[nt] = nti;
  }

  trace("compression")
    << "goto table: from " << (gotoTableSize() * sizeof(GotoEntry))
    << " down to " << (numStates * numColors * sizeof(GotoEntry))
    << " bytes\n";

  // replace the existing table with the compressed one
  delete[] gotoTable;
  gotoTable = newTable;
  gotoCols = numColors;
}


// created by copying 'mergeActionRows' and replacing 'action'
// with 'goto', etc.
void ParseTables::mergeGotoRows()
{
  traceProgress() << "merging goto rows\n";

  // can only do this if we've already pulled out the errors
  xassert(errorBits);

  // for now I assume we don't have a map yet
  xassert(!gotoRowPointers);

  // compute graph of conflicting 'goto' rows
  Bit2d graph(point(numStates, numStates));
  graph.setall(0);

  // fill it in
  for (int s1=0; s1 < numStates; s1++) {
    for (int s2=0; s2 < s1; s2++) {
      // does row 's1' conflict with row 's2'?
      for (int nt=0; nt < gotoCols; nt++) {    // nt is an equivalence class of nonterminals
        GotoEntry g1 = gotoTable[s1*gotoCols + nt];
        GotoEntry g2 = gotoTable[s2*gotoCols + nt];

        if (isErrorGoto(g1) ||
            isErrorGoto(g2) ||
            g1 == g2) {
          // no problem
        }
        else {
          // conflict!
          graph.set(point(s1, s2));
          graph.set(point(s2, s1));
          break;
        }
      }
    }
  }

  // color the graph
  Array<int> color(numStates);      // state -> color (equivalence class)
  int numColors = colorTheGraph(color, graph);

  // build a new, compressed goto table
  GotoEntry *newTable;
  allocInitArray(newTable, numColors * gotoCols, encodeGotoError());

  // merge rows in 'gotoTable' into those in 'newTable'
  // according to the 'color' map

  // gotoTable[]:
  //
  //             t0    t1    t2    t3      // nonterminal equivalence classes
  //   s0
  //   s1
  //   s2
  //    ...
  //   /*states*/

  // newTable[]:
  //
  //             t0    t1    t2    t3      // nonterminal equivalence classes
  //   c0
  //   c1
  //   c2    < e.g., union of state1 and state4 (color[1]==color[4]==2) >
  //    ...
  //   /*state equivalence classes (colors)*/

  gotoRowPointers = new GotoEntry* [numStates];
  for (int s=0; s<numStates; s++) {
    int c = color[s];

    // merge gotoTable row 's' into newTable row 'c'
    for (int nt=0; nt<gotoCols; nt++) {
      GotoEntry &dest = newTable[c*gotoCols + nt];

      GotoEntry src = gotoTable[s*gotoCols + nt];
      if (!isErrorGoto(src)) {
        // make sure there's no conflict (otherwise the graph
        // coloring algorithm screwed up)
        xassert(isErrorGoto(dest) ||
                dest == src);

        // merge the entry
        dest = src;
      }
    }

    // fill in the row pointer map
    gotoRowPointers[s] = newTable + c*gotoCols;
  }

  trace("compression")
    << "goto table: from " << (numStates * gotoCols * sizeof(GotoEntry))
    << " down to " << (numColors * gotoCols * sizeof(GotoEntry))
    << " bytes\n";

  // replace the existing table with the compressed one
  delete[] gotoTable;
  gotoTable = newTable;
  gotoRows = numColors;
}


static int intCompare(void const *left, void const *right)
{
  return *((int const*)left) - *((int const*)right);
}

int ParseTables::colorTheGraph(int *color, Bit2d &graph)
{
  int n = graph.Size().x;  // same as y

  if (tracingSys("graphColor") && n < 20) {
    graph.print();
  }

  // node -> # of adjacent nodes
  Array<int> degree(n);
  memset((int*)degree, 0, n * sizeof(int));

  // node -> # of adjacent nodes that have colors already
  Array<int> blocked(n);

  // initialize some arrays
  enum { UNASSIGNED = -1 };
  {
    for (int i=0; i<n; i++) {
      // clear the color map
      color[i] = UNASSIGNED;
      blocked[i] = 0;

      for (int j=0; j<n; j++) {
        if (graph.get(point(i,j))) {
          degree[i]++;
        }
      }
    }
  }

  // # of colors used
  int usedColors = 0;

  for (int numColored=0; numColored < n; numColored++) {
    // Find a vertex to color.  Prefer nodes that are more constrained
    // (have more blocked colors) to those that are less constrained.
    // Then, prefer those that are least constraining (heave least
    // uncolored neighbors) to those that are more constraining.  If
    // ties remain, choose arbitrarily.
    int best = -1;
    int bestBlocked = 0;
    int bestUnblocked = 0;

    for (int choice = 0; choice < n; choice++) {
      if (color[choice] != UNASSIGNED) continue;

      int chBlocked = blocked[choice];
      int chUnblocked = degree[choice] - blocked[choice];
      if (best == -1 ||                          // no choice yet
          chBlocked > bestBlocked ||             // more constrained
          (chBlocked == bestBlocked &&
           chUnblocked < bestUnblocked)) {       // least constraining
        // new best
        best = choice;
        bestBlocked = chBlocked;
        bestUnblocked = chUnblocked;
      }
    }

    // get the assigned colors of the adjacent vertices
    Array<int> adjColor(bestBlocked);
    int adjIndex = 0;
    for (int i=0; i<n; i++) {
      if (graph.get(point(best,i)) &&
          color[i] != UNASSIGNED) {
        adjColor[adjIndex++] = color[i];
      }
    }
    xassert(adjIndex == bestBlocked);

    // sort them
    qsort((int*)adjColor, bestBlocked, sizeof(int), intCompare);

    // select the lowest-numbered color that won't conflict
    int selColor = 0;
    for (int j=0; j<bestBlocked; j++) {
      if (selColor == adjColor[j]) {
        selColor++;
      }
      else if (selColor < adjColor[j]) {
        // found one that doesn't conflict
        break;
      }
      else {
        // happens when we have two neighbors that have the same color;
        // that's fine, we'll go around the loop again to see what the
        // next neighbor has to say
      }
    }

    // assign 'selColor' to 'best'
    color[best] = selColor;
    if (selColor+1 > usedColors) {
      usedColors = selColor+1;
    }

    // update 'blocked[]'
    for (int k=0; k<n; k++) {
      if (graph.get(point(best,k))) {
        // every neighbor of 'k' now has one more blocked color
        blocked[k]++;
      }
    }
  }

  std::ostream &os = trace("graphColor") << "colors[]:";

  for (int i=0; i<n; i++) {
    // every node should now have blocked == degree
    xassert(blocked[i] == degree[i]);

    // and have a color assigned
    xassert(color[i] != UNASSIGNED);
    os << " " << color[i];
  }

  os << "\n";

  return usedColors;
}


// --------------------- table emission -------------------
// create literal tables
template <class EltType>
void emitTable(EmitCode &out, EltType const *table, int size, int rowLength,
               char const *typeName, char const *tableName)
{
  if (!table || !size) {
    out << "  " << typeName << " *" << tableName << " = NULL;\n";
    return;
  }

  bool printHex = 0==strcmp(typeName, "ErrorBitsEntry") ||
                  (ENABLE_CRS_COMPRESSION && 0==strcmp(typeName, "ActionEntry")) ||
                  (ENABLE_CRS_COMPRESSION && 0==strcmp(typeName, "GotoEntry")) ;
  bool needCast = 0==strcmp(typeName, "StateId");

  if (size * sizeof(*table) > 50) {    // suppress small ones
    out << "  // storage size: " << size * sizeof(*table) << " bytes\n";
    if (size % rowLength == 0) {
      out << "  // rows: " << (size/rowLength) << "  cols: " << rowLength << "\n";
    }
  }

  int rowNumWidth = sm_stringf("%d", size / rowLength /*round down*/).length();

  // I make tables 'const' because that way the OS loader might be
  // smart enough to share them (on a read-only basis) across multiple
  // processes started from the same executable.  But I immediately
  // cast them to non-const, since ParseTables doesn't declare
  // pointers-to-const (since it also has methods to modify the tables
  // at parser generation time).

  out << "  static " << typeName << " const " << tableName << "[" << size << "] = {";
  int row = 0;
  for (int i=0; i<size; i++) {
    if (i % rowLength == 0) {    // one row per state
      out << sm_stringf("\n    /""*%*d*""/ ", rowNumWidth, row++);
    }

    if (needCast) {
      out << "(" << typeName << ")";
    }

    if (printHex) {
      out << sm_stringf("0x%02X, ", table[i]);
    }
    else if (sizeof(table[i]) == 1) {
      // little bit of a hack to make sure 'unsigned char' gets
      // printed as an int; the casts are necessary because this
      // code gets compiled even when EltType is ProdInfo
      out << (int)(*((unsigned char*)(table+i))) << ", ";
    }
    else {
      // print the other int-sized things, or ProdInfo using
      // the overloaded '<<' below
      out << table[i] << ", ";
    }
  }
  out << "\n"
      << "  };\n";
}

// used to emit the elements of the prodInfo table
sm_stringBuilder& operator<< (sm_stringBuilder &sb, ParseTables::ProdInfo const &info)
{
  sb << "{" << (int)info.rhsLen << "," << (int)info.lhsIndex << "}";
  return sb;
}


// like 'emitTable', but also set a local called 'tableName'
template <class EltType>
void emitTable2(EmitCode &out, EltType const *table, int size, int rowLength,
                char const *typeName, char const *tableName)
{
  sm_string tempName = sm_stringc << tableName << "_static";
  emitTable(out, table, size, rowLength, typeName, tempName);
  out << "  " << tableName << " = const_cast<" << typeName << "*>("
      << tempName << ");\n\n";
}


template <class EltType>
void emitOffsetTable(EmitCode &out, EltType **table, EltType *base, int size,
                     char const *typeName, char const *tableName, char const *baseName)
{
  if (!table) {
    out << "  " << tableName << " = NULL;\n\n";
    return;
  }

  // make the pointers persist by storing a table of offsets
  Array<int> offsets(size);
  bool allUnassigned = true;
  for (int i=0; i < size; i++) {
    if (table[i]) {
      offsets[i] = table[i] - base;
      allUnassigned = false;
    }
    else {
      offsets[i] = UNASSIGNED;    // codes for a NULL entry
    }
  }

  if (allUnassigned) {
    // for example, an LALR(1) grammar has no ambiguous entries in its tables
    size = 0;
  }

  if (size > 0) {
    out << "  " << tableName << " = new " << typeName << " [" << size << "];\n";

    emitTable(out, (int*)offsets, size, 16, "int", sm_stringc << tableName << "_offsets");

    // at run time, interpret the offsets table
    out << "  for (int i=0; i < " << size << "; i++) {\n"
        << "    int ofs = " << tableName << "_offsets[i];\n"
        << "    if (ofs >= 0) {\n"
        << "      " << tableName << "[i] = " << baseName << " + ofs;\n"
        << "    }\n"
        << "    else {\n"
        << "      " << tableName << "[i] = NULL;\n"
        << "    }\n"
        << "  }\n\n";
  }
  else {
    out << "  // offset table is empty\n"
        << "  " << tableName << " = NULL;\n\n";
  }
}


// for debugging
template <class EltType>
void printTable(EltType const *table, int size, int rowLength,
                char const *typeName, char const *tableName)
{
  // disabled for now since I don't need it anymore, and it adds
  // a link dependency on emitcode.cc ...
  #if 0
  {
    EmitCode out("printTable.tmp");
    emitTable(out, table, size, rowLength, typeName, tableName);
  }

  system("cat printTable.tmp; rm printTable.tmp");
  #endif // 0
}


// emit code for a function which, when compiled and executed, will
// construct this same table (except the constructed table won't own
// the table data, since it will point to static program data)
void ParseTables::emitConstructionCode(EmitCode &out,
  char const *className, char const *funcName)
{
  // must have already called 'finishTables'
  xassert(!temp);

  out << "// this makes a ParseTables from some literal data;\n"
      << "// the code is written by ParseTables::emitConstructionCode()\n"
      << "// in " << __FILE__ << "\n"
      << "class " << className << "_ParseTables : public ParseTables {\n"
      << "public:\n"
      << "  " << className << "_ParseTables();\n"
      << "};\n"
      << "\n"
      << className << "_ParseTables::" << className << "_ParseTables()\n"
      << "  : ParseTables(false /*owning*/)\n"
      << "{\n"
      ;

  // set all the integer-like variables
  #define SET_VAR(var) \
    out << "  " #var " = " << var << ";\n";
  SET_VAR(numTerms);
  SET_VAR(numNonterms);
  SET_VAR(numStates);
  SET_VAR(numProds);
  SET_VAR(actionCols);
  SET_VAR(actionRows);
  SET_VAR(gotoCols);
  SET_VAR(gotoRows);
  SET_VAR(ambigTableSize);
  out << "  startState = (StateId)" << (int)startState << ";\n";
  SET_VAR(finalProductionIndex);
  SET_VAR(bigProductionListSize);
  SET_VAR(errorBitsRowSize);
  SET_VAR(uniqueErrorRows);
  #undef SET_VAR
  out << "\n";

  // action table, one row per state
  emitTable2(out, actionTable, actionTableSize(), actionCols,
             "ActionEntry", "actionTable");

  // goto table, one row per state
  emitTable2(out, gotoTable, gotoTableSize(), gotoCols,
             "GotoEntry", "gotoTable");

  // production info, arbitrarily 16 per row
  emitTable2(out, prodInfo, numProds, 16, "ParseTables::ProdInfo", "prodInfo");

  // state symbol map, arbitrarily 16 per row
  emitTable2(out, stateSymbol, numStates, 16, "SymbolId", "stateSymbol");

  // ambigTable
  emitTable2(out, ambigTable, ambigTableSize, 16, "ActionEntry", "ambigTable");

  // nonterminal order
  emitTable2(out, nontermOrder, nontermOrderSize(), 16,
             "NtIndex", "nontermOrder");

  // errorBits
  emitTable2(out, errorBits, uniqueErrorRows * errorBitsRowSize, errorBitsRowSize,
             "ErrorBitsEntry", "errorBits");

  emitOffsetTable(out, errorBitsPointers, errorBits, numStates,
                  "ErrorBitsEntry*", "errorBitsPointers", "errorBits");

  // actionIndexMap
  emitTable2(out, actionIndexMap, numTerms, 16,
             "TermIndex", "actionIndexMap");

  // actionRowPointers
  emitOffsetTable(out, actionRowPointers, actionTable, numStates,
                  "ActionEntry*", "actionRowPointers", "actionTable");

  // gotoIndexMap
  emitTable2(out, gotoIndexMap, numNonterms, 16,
             "NtIndex", "gotoIndexMap");

  // gotoRowPointers
  emitOffsetTable(out, gotoRowPointers, gotoTable, numStates,
                  "GotoEntry*", "gotoRowPointers", "gotoTable");

  if (ENABLE_CRS_COMPRESSION) {
    emitTable2(out, firstWithTerminal, numTerms, 16,
               "StateId", "firstWithTerminal");

    emitTable2(out, firstWithNonterminal, numNonterms, 16,
               "StateId", "firstWithNonterminal");

    emitTable2(out, bigProductionList, bigProductionListSize, 16,
               "ProdIndex", "bigProductionList");

    emitOffsetTable(out, productionsForState, bigProductionList, numStates,
                    "ProdIndex*", "productionsForState", "bigProductionList");

    emitOffsetTable(out, ambigStateTable, ambigTable, numStates,
                    "ActionEntry*", "ambigStateTable", "ambigTable");
  }
  else {
    out << "  firstWithTerminal = NULL;\n"
        << "  firstWithNonterminal = NULL;\n"
        << "  bigProductionList = NULL;\n"
        << "  productionsForState = NULL;\n"
        << "  ambigStateTable = NULL;\n"
        ;
  }

  out << "}\n"
      << "\n"
      << "\n"
      << "ParseTables *" << className << "::" << funcName << "()\n"
      << "{\n"
      << "  return new " << className << "_ParseTables;\n"
      << "}\n"
      << "\n"
      ;
}


// EOF
