// gramanl.h            see license.txt for copyright and terms of use
// grammar analysis module; separated from grammar.h to
//   reduce mixing of representation and algorithm; this
//   module should be entirely algorithm

// Author: Scott McPeak, April 2000
// Updates: March 2002

// references:
//
//   [ASU]  Aho, Sethi Ullman.  Compilers: Principles,
//          Techniques, and Tools.  Addison-Wesley,
//          Reading, MA.  1986.  Second printing (3/88).
//          [A classic reference for LR parsing.]


#ifndef __GRAMANL_H
#define __GRAMANL_H

#include "elk_grammar.h"
#include "sm_ohashtbl.h"
#include "sm_okhashtbl.h"
#include "sm_okhasharr.h"
#include "elk_glrconfig.h"
#include "elk_parsetables.h"

// forward decls
class Bit2d;              // bit2d.h
class BitArray;           // bitarray.h
class EmitCode;           // emitcode.h

// this file
class GrammarAnalysis;


// ---------------- DottedProduction --------------------
// a production, with an indicator that says how much of this
// production has been matched by some part of the input sm_string
// (exactly which part of the input depends on where this appears
// in the algorithm's data structures)
class DottedProduction {
// ------ representation ------
private:    // data
  Production const *prod;        // (serf) the base production
  int dot;                       // 0 means it's before all RHS symbols, 1 means after first, etc.

// -------- annotation ----------
private:    // data
  // performance optimization: NULL if dot at end, or else pointer
  // to the symbol right after the dot
  Symbol *afterDot;

public:     // data
  // First of the sentential form that follows the dot; this set
  // is computed by GrammarAnalysis::computeDProdFirsts
  TerminalSet firstSet;

  // also computed by computeDProdFirsts, this is true if the
  // sentential form can derive epsilon (the empty sm_string)
  bool canDeriveEmpty;

  // during item set closure, I need a way to map from dotted prods to
  // the items which use them; so rather than use a hash table, I'll
  // just annotate the dprods themselves with backpointers; these
  // backpointers *must* be maintained as NULL when there's no
  // association
  mutable class LRItem *backPointer;

private:    // funcs
  void init();

public:     // funcs
  //DottedProduction(DottedProduction const &obj);

  // need the grammar passed during creation so we know how big
  // to make 'lookahead'
  //DottedProduction(GrammarAnalysis const &g);       // for later filling-in
  //DottedProduction(/*GrammarAnalysis const &g,*/ Production *p, int d);
  DottedProduction();     // for creating arrays of them
  ~DottedProduction();

  // no point to flattening these because they're easily re-computable
  #if 0
  DottedProduction(Flatten&);
  void xfer(Flatten &flat);
  void xferSerfs(Flatten &flat, GrammarAnalysis &g);
  #endif // 0

  // simple queries
  Production const *getProd() const { return prod; }
  int getDot() const { return dot; }
  bool isDotAtStart() const { return dot==0; }
  bool isDotAtEnd() const { return afterDot==NULL; }

  // no need for equality now, since all DPs with the same
  // prod/dot are shared
  //bool isEqual(DottedProduction const &obj) const;
  //bool operator== (DottedProduction const &obj) const;

  // call this to change prod and dot
  void setProdAndDot(Production const *p, int d);

  // dot must not be at the start (left edge)
  Symbol const *symbolBeforeDotC() const;
  Symbol *symbolBeforeDot() { return const_cast<Symbol*>(symbolBeforeDotC()); }

  // dot must not be at the end (right edge)
  Symbol const *symbolAfterDotC() const { return afterDot; }
  Symbol *symbolAfterDot() { return const_cast<Symbol*>(symbolAfterDotC()); }

  // print to std::cout as 'A -> B . c D' (no newline)
  void print(std::ostream &os/*, GrammarAnalysis const &g*/) const;
  OSTREAM_OPERATOR(DottedProduction)
};

// lists of dotted productions
typedef ObjList<DottedProduction> DProductionList;
typedef ObjListIter<DottedProduction> DProductionListIter;
typedef SObjList<DottedProduction> SDProductionList;
typedef SObjListIter<DottedProduction> SDProductionListIter;

#define FOREACH_DOTTEDPRODUCTION(list, iter) FOREACH_OBJLIST(DottedProduction, list, iter)
#define MUTATE_EACH_DOTTEDPRODUCTION(list, iter) MUTATE_EACH_OBJLIST(DottedProduction, list, iter)
#define SFOREACH_DOTTEDPRODUCTION(list, iter) SFOREACH_OBJLIST(DottedProduction, list, iter)
#define SMUTATE_EACH_DOTTEDPRODUCTION(list, iter) SMUTATE_EACH_OBJLIST(DottedProduction, list, iter)


// --------------- LRItem ---------------
// a dotted production with a lookahead; whereas each production
// has a fixed number of dotted versions of that production, there
// can be lots of items, because of the differing lookahead sets
// (I prefer the name "LRItem" to simply "Item" because the latter
// easily collides with other uses)
class LRItem {
public:    // data
  DottedProduction const *dprod;  // (serf) production and dot position
  TerminalSet lookahead;          // lookahead symbols

public:    // funcs
  LRItem(LRItem const &obj);
  ~LRItem();

  // need 'numTerms' to tell how big to make 'lookahead'
  LRItem(int numTerms, DottedProduction const *dp);

  LRItem(Flatten&);
  void xfer(Flatten &flat);
  void xferSerfs(Flatten &flat, GrammarAnalysis &g);

  // comparison
  static int diff(LRItem const *a, LRItem const *b, void*);
  bool equalNoLA(LRItem const &obj) const
    { return dprod == obj.dprod; }

  // manipulate the lookahead set
  bool laContains(int terminalId) const
    { return lookahead.contains(terminalId); }
  void laAdd(int terminalId)
    { lookahead.add(terminalId); }
  void laRemove(int terminalId)
    { lookahead.remove(terminalId); }
  void laCopy(LRItem const &obj)
    { lookahead.copy(obj.lookahead); }
  bool laMerge(LRItem const &obj)     // returns true if merging changed lookahead
    { return lookahead.merge(obj.lookahead); }
  bool laIsEqual(LRItem const &obj) const
    { return lookahead.isEqual(obj.lookahead); }

  // pass-thru queries into 'dprod'
  Production const *getProd() const
    { return dprod->getProd(); }
  int getDot() const
    { return dprod->getDot(); }
  bool isDotAtStart() const
    { return dprod->isDotAtStart(); }
  bool isDotAtEnd() const
    { return dprod->isDotAtEnd(); }
  Symbol const *symbolBeforeDotC() const
    { return dprod->symbolBeforeDotC(); }
  Symbol const *symbolAfterDotC() const
    { return dprod->symbolAfterDotC(); }

  int prodIndex() const
    { return getProd()->prodIndex; }

  // stuff for insertion into a hash table
  static unsigned hash(DottedProduction const *key);
  static DottedProduction const *dataToKey(LRItem *dp);
  static bool dpEqual(DottedProduction const *key1, DottedProduction const *key2);

  // true if this item is "A -> alpha * t beta"
  bool isExtendingShift(Nonterminal const *A, Terminal const *t) const;

  void print(std::ostream &os, GrammarAnalysis const &g) const;
};


// ---------------- ItemSet -------------------
// a set of dotted productions, and the transitions between
// item sets, as in LR(0) set-of-items construction
class ItemSet {
public:     // intended to be read-only public
  // kernel items: the items that define the set; except for
  // the special case of the initial item in the initial state,
  // the kernel items are distinguished by having the dot *not*
  // at the left edge
  ObjList<LRItem> kernelItems;

  // nonkernel items: those derived as the closure of the kernel
  // items by expanding symbols to the right of dots; here I am
  // making the choice to materialize them, rather than derive
  // them on the spot as needed (and may change this decision)
  ObjList<LRItem> nonkernelItems;

private:    // data
  // transition function (where we go on shifts); NULL means no transition
  //   Map : (Terminal id or Nonterminal id)  -> ItemSet*
  ItemSet **termTransition;                  // (owner ptr to array of serf ptrs)
  ItemSet **nontermTransition;               // (owner ptr to array of serf ptrs)

  // bounds for above
  int terms;
  int nonterms;

  // profiler reports I'm spending significant time rifling through
  // the items looking for those that have the dot at the end; so this
  // array will point to all such items
  LRItem const **dotsAtEnd;                  // (owner ptr to array of serf ptrs)
  int numDotsAtEnd;                          // number of elements in 'dotsAtEnd'

  // profiler also reports I'm still spending time comparing item sets; this
  // stores a CRC of the numerically sorted kernel item pointer addresses,
  // concatenated into a buffer of sufficient size
  unsigned long kernelItemsCRC;

  // need to store this, because I can't compute it once I throw
  // away the items
  Symbol const *stateSymbol;

public:     // data
  // numerical state id, should be unique among item sets
  // in a particular grammar's sets
  StateId id;

  // it's useful to have a BFS tree superimposed on the transition
  // graph; for example, it makes it easy to generate sample inputs
  // for each state.  so we store the parent pointer; we can derive
  // child pointers by looking at all outgoing transitions, and
  // filtering for those whose targets' parent pointers equal 'this'.
  // the start state's parent is NULL, since it is the root of the
  // BFS tree
  ItemSet *BFSparent;                        // (serf)

private:    // funcs
  int bcheckTerm(int index) const;
  int bcheckNonterm(int index) const;
  ItemSet *&refTransition(Symbol const *sym);

  void allocateTransitionFunction();
  Symbol const *computeStateSymbolC() const;

  void deleteNonReductions(ObjList<LRItem> &list);

public:     // funcs
  ItemSet(StateId id, int numTerms, int numNonterms);
  ~ItemSet();

  ItemSet(Flatten&);
  void xfer(Flatten &flat);
  void xferSerfs(Flatten &flat, GrammarAnalysis &g);

  // ---- item queries ----
  // the set of items names a symbol as the symbol used
  // to reach this state -- namely, the symbol that appears
  // to the left of a dot.  this fn retrieves that symbol
  // (if all items have dots at left edge, returns NULL; this
  // would be true only for the initial state)
  Symbol const *getStateSymbolC() const { return stateSymbol; }

  // equality is defined as having the same items (basic set equality)
  bool operator== (ItemSet const &obj) const;

  // sometimes it's convenient to have all items mixed together
  // (CONSTNESS: allows modification of items...)
  void getAllItems(SObjList<LRItem> &dest, bool nonkernel=true) const;

  // used for sorting by id
  static int diffById(ItemSet const *left, ItemSet const *right, void*);

  // ---- transition queries ----
  // query transition fn for an arbitrary symbol; returns
  // NULL if no transition is defined
  ItemSet const *transitionC(Symbol const *sym) const;
  ItemSet *transition(Symbol const *sym)
    { return const_cast<ItemSet*>(transitionC(sym)); }

  // alternate interface; also might return NULL
  ItemSet const *getTermTransition(int termId) const
    { return termTransition[bcheckTerm(termId)]; }
  ItemSet const *getNontermTransition(int nontermId) const
    { return nontermTransition[bcheckNonterm(nontermId)]; }

  // get the list of productions that are ready to reduce, given
  // that the next input symbol is 'lookahead' (i.e. in the follow
  // of a production's LHS); parsing=true means we are actually
  // parsing input, so certain tracing output is appropriate;
  // 'reductions' is a list of const Productions
  void getPossibleReductions(ProductionList &reductions,
                             Terminal const *lookahead,
                             bool parsing) const;


  // assuming this itemset has at least one reduction ready (an assertion
  // checks this), retrieve the first one
  Production const *getFirstReduction() const;

  // ---- item mutations ----
  // add a kernel item; used while constructing the state
  void addKernelItem(LRItem * /*owner*/ item);

  // after adding all kernel items, call this
  void sortKernelItems();

  // add a nonkernel item; used while computing closure; this
  // item must not already be in the item set
  void addNonkernelItem(LRItem * /*owner*/ item);

  // computes things derived from the item set lists:
  // dotsAtEnd, numDotsAtEnd, kernelItemsCRC, stateSymbol;
  // do this after adding things to the items lists
  void changedItems();

  // a part of 'changedItems', this is used in a specialized way
  // during LR item set construction; it leaves 'this' in a somewhat
  // half-baked state (if changedItems is not also called), so some
  // care needs to be taken when using this directly
  void computeKernelCRC(GrowArray<DottedProduction const*> &array);

  // remove the reduce using 'prod' on lookahead 'sym;
  // calls 'changedItems' internally
  void removeReduce(Production const *prod, Terminal const *sym);

  // throw away information not needed during parsing
  void throwAwayItems();

  // 'dest' has already been established to have the same kernel
  // items as 'this' -- so merge all the kernel lookahead items
  // of 'this' into 'dest'; return 'true' if any changes were made
  // to 'dest'
  bool mergeLookaheadsInto(ItemSet &dest) const;

  // true if this itemset has an item "A -> alpha * t beta", i.e.
  // one that would extend 'A' by shifting 't'
  bool hasExtendingShift(Nonterminal const *A, Terminal const *t) const;

  // ---- transition mutations ----
  // set transition on 'sym' to be 'dest'
  void setTransition(Symbol const *sym, ItemSet *dest);

  // remove the the shift on 'sym'
  void removeShift(Terminal const *sym);

  // ------ hashtable stuff --------
  static ItemSet const *dataToKey(ItemSet *data);
  static unsigned hash(ItemSet const *key);
  static bool equalKey(ItemSet const *key1, ItemSet const *key2);

  // ---- debugging ----
  void writeGraph(std::ostream &os, GrammarAnalysis const &g) const;
  void print(std::ostream &os, GrammarAnalysis const &g, bool nonkernel=true) const;
};


// ---------------------- GrammarAnalysis -------------------
class GrammarAnalysis : public Grammar {
protected:  // data
  // if entry i,j is true, then nonterminal i can derive nonterminal j
  // (this is a graph, represented (for now) as an adjacency matrix)
  enum { emptyStringIndex = 0 };
  Bit2d *derivable;                     // (owner)

  // index the symbols on their integer ids
  Nonterminal **indexedNonterms;        // (owner -> serfs) ntIndex -> Nonterminal
  Terminal **indexedTerms;              // (owner -> serfs) termIndex -> Terminal
  // numNonterms==Grammar::numNonterminals(), numTerms==Grammar::numTerminals()
  int numNonterms;                      // length of 'indexedNonterms' array
  int numTerms;                         //   "     "         terms       "

  // during itemSetClosure, profiling reports we spend a lot of time
  // walking the list of productions looking for those that have a given
  // symbol on the LHS; so let's index produtions by LHS symbol index;
  // this array has 'numNonterms' elements, mapping each nonterminal to
  // the list of productions with that nonterminal on the LHS
  SObjList<Production> *productionsByLHS;    // (owner ptr to array)

  // map of production x dotPosition -> DottedProduction;
  // each element of the 'dottedProds' array is a pointer to an
  // array of DottedProduction objects
  DottedProduction **dottedProds;       // (owner ptr to array of owners)

  // index of productions by id
  Production **indexedProds;            // (owner -> serfs) prodIndex -> Production
  int numProds;                         // length of 'dottedProds'

  // only true after initializeAuxData has been called
  bool initialized;

  // used to assign itemset ids while the item sets are being
  // initially constructed; later, they get renumbered into a
  // canonical order
  int nextItemSetId;

  // the LR parsing tables
  ObjList<ItemSet> itemSets;

  // distinguished start state; NOTE: much of the grammar analysis
  // code currently assumes (and checks) that state 0 is the start
  // state, so if you want to do something different, that code might
  // need to be changed
  ItemSet *startState;                  // (serf)

public:     // data
  // true if any nonterminal can derive itself (with no extra symbols
  // surrounding it) in 1 or more steps
  bool cyclic;

  // symbol of interest; various diagnostics are printed when
  // certain things happen with it (e.g. the first application
  // is to print whenever something is added to this sym's
  // follow)
  Symbol const *symOfInterest;

  // incremented each time we encounter an error that we can recover from
  int errors;

  // parse tables
  ParseTables *tables;                  // (owner)

private:    // funcs
  // ---- analyis init ----
  // call this after grammar is completely built
  void initializeAuxData();
  void computeIndexedNonterms();
  void computeIndexedTerms();
  void computeProductionsByLHS();
  void computeReachable();
  void computeReachableDFS(Nonterminal *nt);
  void resetFirstFollow();
  void computeDProdFirsts();
  void computeSupersets();

  // ---- dotted productions ----
  void createDottedProductions();
  void deleteDottedProductions();
  DottedProduction const *getDProd(Production const *prod, int posn) const;
  DottedProduction *getDProd_nc(Production const *prod, int posn)
    { return const_cast<DottedProduction*>(getDProd(prod, posn)); }

  // given a dprod, yield the one obtained by moving the dot one
  // place to the right
  DottedProduction const *nextDProd(DottedProduction const *dp) const
    #ifdef NDEBUG
      { return dp+1; }      // take advantage of physical co-location
    #endif
      ;                     // debug version checks bounds

  // ---- derivability ----
  // iteratively compute every pair A,B such that A can derive B
  void computeWhatCanDeriveWhat();
  void initDerivableRelation();

  // add a derivability relation; returns true if this makes a change
  bool addDerivable(Nonterminal const *left, Nonterminal const *right);
  bool addDerivable(int leftNtIndex, int rightNtIndex);

  // private derivability interface
  bool canDerive(int leftNtIndex, int rightNtIndex) const;
  bool sequenceCanDeriveEmpty(RHSEltList const &list) const;
  bool iterSeqCanDeriveEmpty(RHSEltListIter iter) const;

  // ---- First ----
  void computeFirst();
  //bool addFirst(Nonterminal *NT, Terminal *term);
  void firstOfSequence(TerminalSet &destList, RHSEltList const &sequence);
  void firstOfIterSeq(TerminalSet &destList, RHSEltListIter sym);

  // ---- Follow ----
  void computeFollow();
  //bool addFollow(Nonterminal *NT, Terminal *term);

  // ---- LR item sets ----
  ItemSet *makeItemSet();
  void disposeItemSet(ItemSet *is);
  void moveDotNoClosure(ItemSet const *source, Symbol const *symbol,
                        ItemSet *dest, ObjList<LRItem> &unusedTail,
                        GrowArray<DottedProduction const*> &array);
  ItemSet *findItemSetInList(ObjList<ItemSet> &list,
                             ItemSet const *itemSet);
  static bool itemSetsEqual(ItemSet const *is1, ItemSet const *is2);

  void constructLRItemSets();
  void lrParse(char const *input);

  void handleShiftReduceConflict(
    bool &keepShift, bool &keepReduce, bool &dontWarn,
    ItemSet const *state, Production const *prod, Terminal const *sym);

  void resolveConflicts(
    ItemSet const *state,        // parse state in which the actions are possible
    Terminal const *sym,         // lookahead symbol for these actions
    ItemSet const *&shiftDest,   // (inout) if non-NULL, the state to which we can shift
    ProductionList &reductions,  // (inout) list of possible reductions
    bool allowAmbig,             // if false, always return at most 1 action
    bool &printedConflictHeader, // (inout) true once we've printed the state header
    int &sr, int &rr);           // (inout) counts of S/R and R/R conflicts, resp.
  void computeParseTables(bool allowAmbig);

  int subsetDirectiveResolution(
    ItemSet const *state,        // parse state in which the actions are possible
    Terminal const *sym,         // lookahead symbol for these actions
    ProductionList &reductions); // list to try to cut down

  void renumberStates();
  static int renumberStatesDiff
    (ItemSet const *left, ItemSet const *right, void *vgramanl);
  static int arbitraryProductionOrder
    (Production const *left, Production const *right, void*);
  static int arbitraryRHSEltOrder
    (Production::RHSElt const *left, Production::RHSElt const *right, void*);

  void computeBFSTree();

  // misc
  void computePredictiveParsingTable();
    // non-const because have to add productions to lists

  void topologicalSort(NtIndex *order,  int &nextOrdinal,
                       NtIndex current, BitArray &seen);

  // the inverse of transition: map a target state to the symbol that
  // would transition to that state (from the given source state)
  Symbol const *inverseTransitionC(ItemSet const *source,
                                   ItemSet const *target) const;

  // sample input helpers
  void leftContext(SymbolList &output, ItemSet const *state) const;
  bool rewriteAsTerminals(TerminalList &output, SymbolList const &input) const;
  bool rewriteAsTerminalsHelper(TerminalList &output, SymbolList const &input,
                                ProductionList &reductionStack) const;
  bool rewriteSingleNTAsTerminals(TerminalList &output, Nonterminal const *nonterminal,
                                  ProductionList &reductionStack) const;

  // let's try this .. it needs to access 'itemSets'
  friend void ItemSet::xferSerfs(Flatten &flat, GrammarAnalysis &g);

  void singleItemClosure(OwnerKHashTable<LRItem, DottedProduction> &finished,
                         ArrayStack<LRItem*> &worklist,
                         //OwnerKHashArray<LRItem, DottedProduction> &workhash,
                         LRItem const *item, TerminalSet &scratchSet);

public:     // funcs
  GrammarAnalysis();
  ~GrammarAnalysis();

  // access symbols by index
  Terminal const *getTerminal(int index) const;
  Nonterminal const *getNonterminal(int index) const;
  Production const *getProduction(int index) const;

  ItemSet const *getItemSet(int index) const;
  int numItemSets() const { return nextItemSetId; }

  // faster access to counts
  int numTerminals() const { return numTerms; }
  int numNonterminals() const { return numNonterms; }

  // binary read/write
  void xfer(Flatten &flat);

  // essentially, my 'main()' while experimenting
  void exampleGrammar();

  // overrides base class to add a little bit of the
  // annotated info
  void printProductions(std::ostream &os, bool printCode=true) const;

  // print lots of stuff
  void printProductionsAndItems(std::ostream &os, bool printCode=true) const;

  // when grammar is built, this runs all analyses and stores
  // the results in this object's data fields; write the LR item
  // sets to the given file (or don't, if NULL)
  void runAnalyses(char const *setsFname);

  // print the item sets to a stream (optionally include nonkernel items)
  void printItemSets(std::ostream &os, bool nonkernel) const;

  // given a grammar, replace all of its actions with actions that
  // will build a straightforward parse tree using the facilities
  // of ptreenode.h; the rules will need the user to already have
  // done some necessary work in the verbatim preamble, such as
  // #including ptreenode.h
  void addTreebuildingActions();

  // ---- grammar queries ----
  bool canDerive(Nonterminal const *lhs, Nonterminal const *rhs) const;
  bool canDeriveEmpty(Nonterminal const *lhs) const;

  bool firstIncludes(Nonterminal const *NT, Terminal const *term) const;
  bool followIncludes(Nonterminal const *NT, Terminal const *term) const;

  // ---- sample inputs and contexts ----
  sm_string sampleInput(ItemSet const *state) const;
  sm_string leftContextString(ItemSet const *state) const;

  // ---- moved out of private ----
  void itemSetClosure(ItemSet &itemSet);
  DottedProduction const *getDProdIndex(int prodIndex, int posn) const;
};


// in gramexpl.cc: interactive grammar experimentation system
void grammarExplorer(GrammarAnalysis &g);


#endif // __GRAMANL_H
