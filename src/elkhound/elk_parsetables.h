// parsetables.h            see license.txt for copyright and terms of use
// ParseTables, a class to contain the tables need by the
// LR/GLR parsing algorithm

#ifndef PARSETABLES_H
#define PARSETABLES_H

#include "sm_array.h"
#include "elk_glrconfig.h"
#include <iostream>     // std::ostream

class Flatten;            // flatten.h
class EmitCode;           // emitcode.h
class Symbol;             // grammar.h
class Bit2d;              // bit2d.h

class ELK_EXTERN ParseTables;

// integer id for an item-set DFA state; I'm using an 'enum' to
// prevent any other integers from silently flowing into it
enum StateId { STATE_INVALID=-1 };

inline std::ostream& operator<< (std::ostream &os, StateId id)
  { return os << (int)id; }


// encodes an action in 'action' table; see 'actionTable'
#if ENABLE_CRS_COMPRESSION
  // high bits encoding
  enum ActionEntryKind {
    AE_MASK      = 0xC0,    // selection mask
    AE_SHIFT     = 0x00,    // 00 = shift
    AE_REDUCE    = 0x40,    // 01 = reduce
    AE_AMBIGUOUS = 0x80,    // 10 = ambiguous
    AE_ERROR     = 0xC0,    // 11 = error (if EEF is off)
    AE_MAXINDEX  = 63       // maximum value of lower bits
  };

  // remaining 6 bits:
  //
  //   shift: desination state, encoded as an offset from the
  //   first state that that terminal can reach
  //
  //   reduce: production, encoded as an index into a per-state
  //   array of distinct production indices
  //
  //   ambiguous: for each state, have an array of ActionEntries.
  //   ambiguous entries index into this array.  first indexed
  //   entry is the count of how many actions follow
  typedef unsigned char ActionEntry;
  ActionEntry makeAE(ActionEntryKind k, int index);
  #define errorActionEntry ((ActionEntry)AE_ERROR)
#else
  // each entry is one of:
  //   +N+1, 0 <= N < numStates:         shift, and go to state N
  //   -N-1, 0 <= N < numProds:          reduce using production N
  //   numStates+N+1, 0 <= N < numAmbig: ambiguous, use ambigAction N
  //   0:                                error
  // (there is no 'accept', acceptance is handled outside this table)
  typedef signed short ActionEntry;
  #define errorActionEntry ((ActionEntry)0)
#endif


// encodes a destination state in 'gotoTable'
#if ENABLE_CRS_COMPRESSION
  // entry is an offset from the first state that can be reached
  // by shifting the nonterminal
  typedef unsigned char GotoEntry;
#else
  // entry is the to go to after shifting the nonterminal
  typedef unsigned short GotoEntry;
#endif
#define errorGotoEntry ((GotoEntry)~0)


// name a terminal using an index
typedef unsigned char TermIndex;

// name a nonterminal using an index
typedef unsigned char NtIndex;

// name a production using an index
typedef unsigned short ProdIndex;

// an addressed cell in the 'errorBits' table
typedef unsigned char ErrorBitsEntry;


// encodes either terminal index N (as N+1) or
// nonterminal index N (as -N-1), or 0 for no-symbol
typedef signed short SymbolId;
inline bool symIsTerm(SymbolId id) { return id > 0; }
inline int symAsTerm(SymbolId id) { return id-1; }
inline bool symIsNonterm(SymbolId id) { return id < 0; }
inline NtIndex symAsNonterm(SymbolId id) { return (NtIndex)(-(id+1)); }
SymbolId encodeSymbolId(Symbol const *sym);       // gramanl.cc


// assign, but check for truncation
template <class DEST, class SRC>
inline void checkAssign(DEST &d, SRC s)
{
  d = (DEST)s;
  xassert(d == s);
}


// the parse tables are the traditional action/goto, plus the list
// of ambiguous actions, plus any more auxilliary tables useful during
// run-time parsing
class ELK_EXTERN ParseTables {
private:    // types
  // data about an intermediate state of parse table construction;
  // once the table is finished, this data gets consolidated into the
  // actual tables, and then thrown away
  class TempData {
  public:   // data
    // nascent ambigTable
    ArrayStack<ActionEntry> ambigTable;

    // nascent bigProductionList
    ArrayStack<ProdIndex> bigProductionList;

    // nascent productionsForState, except using integer offsets from
    // start of 'bigProductionList' instead of direct pointers into it
    ArrayStack<int> productionsForState;

    // nascent versions of ambig tables, again with integer offsets
    ArrayStack<int> ambigStateTable;

  public:   // funcs
    TempData(int numStates);
    ~TempData();
  };

public:     // types
  // per-production info
  struct ProdInfo {
    unsigned char rhsLen;                // # of RHS symbols
    NtIndex lhsIndex;                    // 'ntIndex' of LHS
  };

protected:  // data
  // when this is false, all of the below "(owner*)" annotations are
  // actually "(serf)", i.e. this object does *not* own any of the
  // tables (see emitConstructionCode())
  bool owning;

  // non-NULL during construction
  TempData *temp;                        // (nullable owner)

  // # terminals, nonterminals in grammar
  int numTerms;
  int numNonterms;

  // # of parse states
  int numStates;

  // # of productions in the grammar
  int numProds;

  // action table, indexed by (state*actionCols + lookahead)
  int actionCols;
  ActionEntry *actionTable;              // (owner*)

  // goto table, indexed by (state*gotoCols + nontermId)
  int gotoCols;
  GotoEntry *gotoTable;                  // (owner*)

  // map production id to information about that production
  ProdInfo *prodInfo;                    // (owner*)

  // map a state id to the symbol (terminal or nonterminal) which is
  // shifted to arrive at that state
  SymbolId *stateSymbol;                 // (owner*)

  // ambiguous actions: one big list, for allocation purposes; then
  // the actions encode indices into this table; the first indexed
  // entry gives the # of actions, and is followed by that many
  // actions, each interpreted the same way ordinary 'actionTable'
  // entries are
  int ambigTableSize;
  ActionEntry *ambigTable;               // (nullable owner*)

  // total order on nonterminals for use in choosing which to
  // reduce to in the RWL algorithm; index into this using a
  // nonterminal index, and it yields the ordinal for that
  // nonterminal (so these aren't really NtIndex's, but they're
  // exactly as wide, so I use NtIndex anyway)
  //
  // The order is consistent with the requirement that if
  //   A ->+ B
  // then B will be earlier in the order (assuming acyclicity).
  // That way, we'll do all reductions to B before any to A (for
  // reductions spanning the same set of ground terminals), and
  // therefore will merge all alternatives for B before reducing
  // any of them to A.
  NtIndex *nontermOrder;                 // (owner*)

  // --------------------- table compression ----------------------

  // table compression techniques taken from:
  //   [DDH] Peter Dencker, Karl Duerre, and Johannes Heuft.
  //   Optimization of Parser Tables for Portable Compilers.
  //   In ACM TOPLAS, 6, 4 (1984) 546-572.
  //   http://citeseer.nj.nec.com/context/27540/0 (not in database)
  //   ~/doc/papers/p546-dencker.pdf (from ACM DL)

  // Code Reduction Scheme (CRS):
  //
  // Part (a):  The states are numbered such that all states that
  // are reached by transitions on a given symbol are contiguous.
  // See gramanl.cc, GrammarAnalysis::renumberStates().  Then, we
  // simply need a map from the symbol index to the first state
  // that is reached along that symbol.
  StateId *firstWithTerminal;            // (nullable owner*) termIndex -> state
  StateId *firstWithNonterminal;         // (nullable owner*) ntIndex -> state
  //
  // Part (b):  The production indices that appear on a given row
  // are collected together.  (This is called (c) by [DDH]; I don't
  // have a counterpart to their (b).)
  int bigProductionListSize;
  ProdIndex *bigProductionList;          // (nullable owner*) array into which 'productionsForState' points
  ProdIndex **productionsForState;       // (nullable owner to serf) state -> stateProdIndex -> prodIndex
  //
  // Part (c):  Pointers into 'ambigTable' are are collected together in
  // per-state lists as well.
  ActionEntry **ambigStateTable;         // (nullable owner) state -> (+ambigStateTableIndex -> ActionEntry*)

  // Error Entry Factoring (EEF):
  //
  // Factor out all the error entries into their own bitmap.  Then
  // regard error entries in the original tables as "insignificant".
  //
  // 'errorBits' is a map of where the error actions are in the action
  // table.  It is indexed through 'errorBitsPointers':
  //   byte = errorBitsPointers[stateId][lookahead >> 3];
  //   if ((byte >> (lookahead & 7)) & 1) then ERROR
  int errorBitsRowSize;                  // bytes per row
  int uniqueErrorRows;                   // distinct rows
  ErrorBitsEntry *errorBits;             // (nullable owner*)
  ErrorBitsEntry **errorBitsPointers;    // (nullable owner ptr to serfs)

  // Graph Coloring Scheme (GCS):
  //
  // Merge lines and columns that have identical significant entries.
  // This is done as two-pass graph coloring.  They give a specific
  // heuristic.
  //
  // this is a map to be applied to terminal indices before being
  // used to access the compressed action table; it maps the terminal
  // id (as reported by the lexer) to the proper action table column
  TermIndex *actionIndexMap;             // (nullable owner*)
  //
  // this is a map from states to the beginning of the action table
  // row that pertains to that state; it effectively factors the
  // states into equivalence classes
  int actionRows;                        // rows in actionTable[]
  ActionEntry **actionRowPointers;       // (nullable owner ptr to serfs)
  //
  // index map for the goto table
  NtIndex *gotoIndexMap;                 // (nullable owner*)
  //
  // row map for the goto table
  int gotoRows;
  GotoEntry **gotoRowPointers;           // (nullable owner ptr to serfs)

public:     // data
  // These are public because if they weren't, I'd just have a stupid
  // getter/setter pattern that exposes them anyway.

  // start state id
  StateId startState;

  // index of the production which will finish a parse; it's the
  // final reduction executed
  int finalProductionIndex;

private:    // funcs
  void alloc(int numTerms, int numNonterms, int numStates, int numProds,
             StateId start, int finalProd);

  // index tables
  ActionEntry &actionEntry(StateId stateId, int termId)
    { return actionTable[stateId*actionCols + termId]; }
  int actionTableSize() const
    { return actionRows * actionCols; }

  GotoEntry &gotoEntry(StateId stateId, int nontermId)
    { return gotoTable[stateId*gotoCols + nontermId]; }
  int gotoTableSize() const
    { return gotoRows * gotoCols; }

  void appendAmbig(ArrayStack<ActionEntry> const &set);
  bool compareAmbig(ArrayStack<ActionEntry> const &set, int startIndex);

  void fillInErrorBits(bool setPointers);
  int colorTheGraph(int *color, Bit2d &graph);

protected:  // funcs
  // the idea is that 'emitConstructionCode' will emit code that
  // defines a subclass of 'ParseTables'; that's why so many of the
  // data members are protected: the subclass can then access them
  // directly, which is very convenient when trying to construct the
  // tables from static data
  ParseTables(bool owning);    // only legal when owning==false

public:     // funcs
  ParseTables(int numTerms, int numNonterms, int numStates, int numProds,
              StateId start, int finalProd);
  ~ParseTables();

  // simple queries
  int getNumTerms() const { return numTerms; }
  int getNumNonterms() const { return numNonterms; }
  int getNumStates() const { return numStates; }
  int getNumProds() const { return numProds; }

  // finish construction; do this before emitting code
  void finishTables();

  // write the tables out as C++ source that can be compiled into
  // the program that will ultimately do the parsing
  void emitConstructionCode(EmitCode &out, char const *className, char const *funcName);

  // this does the same thing for ML, and is implemented in genml.cc
  void emitMLConstructionCode(EmitCode &out, char const *className, char const *funcName);


  // -------------------- table construction ------------------------
  // CRS dest-state origin tables
  void setFirstWithTerminal(int termId, StateId s) {
    xassert((unsigned)termId < (unsigned)numTerms);
    firstWithTerminal[termId] = s;
  }
  void setFirstWithNonterminal(int nontermId, StateId s) {
    xassert((unsigned)nontermId < (unsigned)numNonterms);
    firstWithNonterminal[nontermId] = s;
  }

  void setActionEntry(StateId stateId, int termId, ActionEntry act)
    { actionEntry(stateId, termId) = act; }
  void setGotoEntry(StateId stateId, int nontermId, GotoEntry got)
    { gotoEntry(stateId, nontermId) = got; }

  // encode actions
  ActionEntry encodeShift(StateId destState, int shiftedTermId);
  ActionEntry encodeReduce(int prodId, StateId inWhatState);
  ActionEntry encodeAmbig(ArrayStack<ActionEntry> const &set,
                          StateId inWhatState);
  ActionEntry encodeError() const;
  ActionEntry validateAction(int code) const;

  // encode gotos
  GotoEntry encodeGoto(StateId stateId, int shiftedNontermId) const;
  GotoEntry encodeGotoError() const
    { return errorGotoEntry; }
  GotoEntry validateGoto(int code) const;

  // misc
  void setProdInfo(int prodId, int rhsLen, int ntIndex) {
    checkAssign(prodInfo[prodId].rhsLen, rhsLen);
    checkAssign(prodInfo[prodId].lhsIndex, ntIndex);
  }
  void setStateSymbol(StateId state, SymbolId sym) {
    stateSymbol[state] = sym;
  }
  NtIndex *getWritableNontermOrder() {
    // expose this directly, due to the way the algorithm that
    // computes it is written
    return nontermOrder;
  }

  // table compressors
  void computeErrorBits();
  void mergeActionColumns();
  void mergeActionRows();
  void mergeGotoColumns();
  void mergeGotoRows();


  // -------------------- table queries ---------------------------
  // return true if the action is an error
  bool actionEntryIsError(StateId stateId, int termId) {
    #if ENABLE_EEF_COMPRESSION
      // check with the error table
      return ( errorBitsPointers[stateId][termId >> 3]
                 >> (termId & 7) ) & 1;
    #else
      return isErrorAction(actionEntry(stateId, termId));
    #endif
  }

  // query action table, without checking the error bitmap
  ActionEntry getActionEntry_noError(StateId stateId, int termId) {
    #if ENABLE_GCS_COMPRESSION
      #if ENABLE_GCS_COLUMN_COMPRESSION
        return actionRowPointers[stateId][actionIndexMap[termId]];
      #else
        return actionRowPointers[stateId][termId];
      #endif
    #else
      return actionEntry(stateId, termId);
    #endif
  }

  // query the action table, yielding an action that might be
  // an error action
  ActionEntry getActionEntry(StateId stateId, int termId) {
    #if ENABLE_EEF_COMPRESSION
      if (actionEntryIsError(stateId, termId)) {
        return errorActionEntry;
      }
    #endif

    return getActionEntry_noError(stateId, termId);
  }

  // decode actions
  #if !ENABLE_CRS_COMPRESSION
    bool isShiftAction(ActionEntry code) const
      { return code > 0 && code <= numStates; }
    static StateId decodeShift(ActionEntry code, int /*shiftedTerminal*/)
      { return (StateId)(code-1); }
    static bool isReduceAction(ActionEntry code)
      { return code < 0; }
    static int decodeReduce(ActionEntry code, StateId /*inState*/)
      { return -(code+1); }
    static bool isErrorAction(ActionEntry code)
      { return code == 0; }

    // ambigAction is only other choice; this yields a pointer to
    // an array of actions, the first of which says how many actions
    // there are
    ActionEntry *decodeAmbigAction(ActionEntry code, StateId /*inState*/) const
      { return ambigTable + (code-1-numStates); }

  #else
    static bool isShiftAction(ActionEntry code) {
      return (code & AE_MASK) == AE_SHIFT;
    }
    StateId decodeShift(ActionEntry code, int shiftedTerminal) {
      return (StateId)(firstWithTerminal[shiftedTerminal] + (code & AE_MAXINDEX));
    }
    static bool isReduceAction(ActionEntry code) {
      return (code & AE_MASK) == AE_REDUCE;
    }
    int decodeReduce(ActionEntry code, StateId inState) {
      return productionsForState[inState][code & AE_MAXINDEX];
    }
    static bool isErrorAction(ActionEntry code) {
      return code == AE_ERROR;
    }

    ActionEntry *decodeAmbigAction(ActionEntry code, StateId inState) const {
      return ambigStateTable[inState] + (code & AE_MAXINDEX);
    }
  #endif

  // decode gotos
  GotoEntry getGotoEntry(StateId stateId, int nontermId) {
    #if ENABLE_GCS_COMPRESSION
      #if ENABLE_GCS_COLUMN_COMPRESSION
        return gotoRowPointers[stateId][gotoIndexMap[nontermId]];
      #else
        return gotoRowPointers[stateId][nontermId];
      #endif
    #else
      return gotoEntry(stateId, nontermId);
    #endif
  }

  bool isErrorGoto(GotoEntry code)
    { return code == errorGotoEntry; }

  StateId decodeGoto(GotoEntry code, int shiftedNonterminal) {
    #if ENABLE_CRS_COMPRESSION
      return (StateId)(firstWithNonterminal[shiftedNonterminal] + code);
    #else
      return (StateId)code;
    #endif
  }

  // nonterminal order
  int nontermOrderSize() const
    { return numNonterms; }
  NtIndex getNontermOrdinal(NtIndex idx) const
    { return nontermOrder[idx]; }

  // misc
  ProdInfo const &getProdInfo(int prodIndex) const
    { return prodInfo[prodIndex]; }
  int getStateSymbol(StateId id) const
    { return stateSymbol[id]; }

  // query compression options based on which fields are not NULL; do
  // *not* use the compile-time flags, because we're trying to detect
  // mismatch between compiler flags used at different times
  bool eef_enabled() const
    { return !!errorBits; }
  bool gcs_enabled() const
    { return !!actionRowPointers; }
  bool gcsc_enabled() const
    { return !!actionIndexMap; }
  bool crs_enabled() const
    { return !!firstWithTerminal; }
};


// NOTE: At one point (before 7/27/03), I had the ability to read and
// write parse tables to files, *not* using the C++ compiler to store
// tables as static data.  I removed it because I wasn't using it, and
// it was hindering table evolution.  But as the tables stabilize
// again, if the need arises, one could go get (from CVS) the code
// that did it and fix it up to work again.


#endif // PARSETABLES_H
