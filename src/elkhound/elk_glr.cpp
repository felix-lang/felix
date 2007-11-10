// glr.cc            see license.txt for copyright and terms of use
// code for glr.h

/* Implementation Notes
 *
 * A design point: [GLR] uses more 'global's than I do.  My criteria
 * here is that something should be global (stored in class GLR) if
 * it has meaning between processing of tokens.  If something is only
 * used during the processing of a single token, then I make it a
 * parameter where necessary.
 *
 * Update: I've decided to make 'currentToken' and 'parserWorklist'
 * global because they are needed deep inside of 'glrShiftNonterminal',
 * though they are not needed by the intervening levels, and their
 * presence in the argument lists would therefore only clutter them.
 *
 * (OLD) It should be clear that many factors contribute to this
 * implementation being slow, and I'm going to refrain from any
 * optimization for a bit.
 *
 * UPDATE (3/29/02): I'm now trying to optimize it.  The starting
 * implementation is 300x slower than bison.  Ideal goal is 3x, but
 * more realistic is 10x.
 *
 * UPDATE (8/24/02): It's very fast now; within 3% of Bison for
 * deterministic grammars, and 5x when I disable the mini-LR core.
 *
 * Description of the various lists in play here:
 *
 *   topmostParsers
 *   --------------
 *   The active parsers are at the frontier of the parse tree
 *   space.  It *never* contains more than one stack node with
 *   a given parse state; I call this the unique-state property
 *   (USP).  If we're about to add a stack node with the same
 *   state as an existing node, we merge them (if it's a shift,
 *   we add another leftAdjState; if it's a reduction, we add a
 *   rule node *and* another leftAdjState).
 *
 *   Before a token is processed, topmostParsers contains those
 *   parsers that successfully shifted the previous token.  This
 *   list is then walked to make the initial reduction worklist.
 *
 *   Before the shifts are processed, the topmostParsers list is
 *   cleared.  As each shift is processed, the resulting parser is
 *   added to topmostParsers (modulo USP).
 *
 *   [GLR] calls this "active-parsers"
 *
 *
 * Discussion of path re-examination, called do-limited-reductions by
 * [GLR]:
 *
 * After thinking about this for some time, I have reached the conclusion
 * that the only way to handle the problem is to separate the collection
 * of paths from the iteration over them.
 *
 * Here are several alternative schemes, and the reasons they don't
 * work:
 *
 *   1. [GLR]'s approach of limiting re-examination to those involving
 *      the new link
 *
 *      This fails because it does not prevent re-examined paths
 *      from appearing in the normal iteration also.
 *
 *   2. Modify [GLR] so the new link can't be used after the re-examination
 *      is complete
 *
 *      Then if *another* new link is added, paths involving both new
 *      links wouldn't be processed.
 *
 *   3. Further schemes involving controlling which re-examination stage can
 *      use which links
 *
 *      Difficult to reason about, unclear a correct scheme exists, short
 *      of the full-blown path-listing approach I'm going to take.
 *
 *   4. My first "fix" which assumes there is never more than one path to
 *      a given parser
 *
 *      This is WRONG.  There can be more than one path, even as all such
 *      paths are labeled the same (namely, with the RHS symbols).  Consider
 *      grammar "E -> x | E + E" parsing "x+x+x": both toplevel parses use
 *      the "E -> E + E" rule, and both arrive at the root parser
 *
 * So, the solution I will implement is to collect all paths into a list
 * before processing any of them.  During path re-examination, I also will
 * collect paths into a list, this time only those that involve the new
 * link.
 *
 * This scheme is clearly correct, since path collection cannot be disrupted
 * by the process of adding links, and when links are added, exactly the new
 * paths are collected and processed.  It's easy to see that every path is
 * considered exactly once.
 *
 *
 * MAJOR UPDATE (12/06/02):  I've replaced the state worklist (SWL) core
 * used in all previous GLR implementations with a reduction worklist (RWL)
 * core.  This core is just as fast, but can be implemented to always
 * avoid the yield-then-merge problem for acyclic grammars.
 *
 *
 * Below, parse-tree building activity is marked "TREEBUILD".
 */


#include "elk_glr.h"
#include "sm_strtokp.h"
#include "sm_syserr.h"
#include "sm_trace.h"
#include "sm_strutil.h"
#include "elk_lexerint.h"
#include "sm_test.h"
#include "sm_sobjlist.h"
#include "sm_owner.h"

#include <stdio.h>       // FILE
#include <stdlib.h>      // getenv

// ACTION(..) is code to execute for action trace diagnostics, i.e. "-tr action"
#ifndef ACTION_TRACE
  #define ACTION_TRACE 0
#endif
#if ACTION_TRACE
  #define ACTION(stmt) stmt
  #define TRSACTION(stuff) if (tracingSys("action")) { std::cout << stuff << std::endl; }
#else
  #define ACTION(stmt)
  #define TRSACTION(stuff)
#endif

// TRSPARSE(stuff) traces <stuff> during debugging with -tr parse
#if !defined(NDEBUG)
  #define IF_NDEBUG(stuff)
  #define TRSPARSE(stuff) if (trParse) { trsParse << stuff << std::endl; }
  #define TRSPARSE_DECL(stuff) stuff
#else
  #define IF_NDEBUG(stuff) stuff
  #define TRSPARSE(stuff)
  #define TRSPARSE_DECL(stuff)
#endif

// whether to use the ordinary LR core in addition to the GLR core
#ifndef USE_MINI_LR
  #define USE_MINI_LR 1
#endif

// these disable features of mini-LR for performance testing
#ifndef USE_ACTIONS
  #define USE_ACTIONS 1
#endif
#ifndef USE_RECLASSIFY
  #define USE_RECLASSIFY 1
#endif
#ifndef USE_KEEP
  #define USE_KEEP 1
#endif

// enables tracking of some statistics useful for debugging and profiling
#ifndef DO_ACCOUNTING
  #define DO_ACCOUNTING 1
#endif
#if DO_ACCOUNTING
  #define ACCOUNTING(stuff) stuff
#else
  #define ACCOUNTING(stuff)
#endif

// unroll the inner loop; approx. 3% performance improvement
// update: right now, it actually *costs* about 8%..
#ifndef USE_UNROLLED_REDUCE
  #define USE_UNROLLED_REDUCE 0
#endif

// some things we track..
int parserMerges = 0;
int computeDepthIters = 0;
int totalExtracts = 0;
int multipleDelayedExtracts = 0;

// can turn this on to experiment.. but right now it
// actually makes things slower.. (!)
//#define USE_PARSER_INDEX


// Note on inlining generally: Inlining functions is a very important
// way to improve performance, in inner loops.  However it's easy to
// guess wrong about where and what to inline.  So generally I mark
// things as inline whenver the profiler (gprof) reports:
//   - it's showing up in gprof as a function call (i.e. not already
//     being inlined)
//   - the function that calls it takes significant time
//   - the call itself takes significant time
// All this is obvious, but is worth saying, since otherwise the
// tendency is to inline everything, which is a mistake because it
// makes the system as a whole slower (by wasting space in the I-cache)
// without leaving a clear indicator of who is to blame (it's very
// hard to profile for over-aggressive inlining).


// the transition to array-based implementations requires I specify
// initial sizes
enum {
  // this one does *not* grow as needed (at least not in the mini-LR core)
  MAX_RHSLEN = 30,

  // ----------
  // the settings below here are for initial sizes of growable arrays,
  // and it should be ok in terms of correctness to set them all to 1,
  // which may be a useful thing during debugging to verify

  // this one grows as needed
  TYPICAL_MAX_REDUCTION_PATHS = 5,

  // this is the length to make arrays which hold rhsLen many items
  // typically, but are growable
  INITIAL_RHSLEN_SIZE = 10,
};


// ------------- front ends to user code ---------------
// given a symbol id (terminal or nonterminal), and its associated
// semantic value, yield a description sm_string
sm_string symbolDescription(SymbolId sym, UserActions *user,
                         SemanticValue sval)
{
  if (symIsTerm(sym)) {
    return user->terminalDescription(symAsTerm(sym), sval);
  }
  else {
    return user->nonterminalDescription(symAsNonterm(sym), sval);
  }
}

SemanticValue GLR::duplicateSemanticValue(SymbolId sym, SemanticValue sval)
{
  xassert(sym != 0);

  // 6/23/04: Why did I do this?  Some kind of optimization?  It should
  // at least be documented... and probably removed altogether.
  if (!sval) return sval;

  SemanticValue ret;
  if (symIsTerm(sym)) {
    ret = userAct->duplicateTerminalValue(symAsTerm(sym), sval);
  }
  else {
    ret = userAct->duplicateNontermValue(symAsNonterm(sym), sval);
  }

  TRSACTION("  " << symbolDescription(sym, userAct, ret) <<
            " is DUP of " <<
            symbolDescription(sym, userAct, sval));

  return ret;
}

void deallocateSemanticValue(SymbolId sym, UserActions *user,
                             SemanticValue sval)
{
  xassert(sym != 0);
  TRSACTION("  DEL " << symbolDescription(sym, user, sval));

  if (!sval) return;

  if (symIsTerm(sym)) {
    return user->deallocateTerminalValue(symAsTerm(sym), sval);
  }
  else {
    return user->deallocateNontermValue(symAsNonterm(sym), sval);
  }
}

void GLR::deallocateSemanticValue(SymbolId sym, SemanticValue sval)
{
  ::deallocateSemanticValue(sym, userAct, sval);
}


// ------------------ SiblingLink ------------------
inline SiblingLink::SiblingLink(StackNode *s, SemanticValue sv
                                SOURCELOCARG( SourceLoc L ) )
  : sib(s), sval(sv)
    SOURCELOCARG( loc(L) )
{
  YIELD_COUNT( yieldCount = 0; )
}

SiblingLink::~SiblingLink()
{}


// ----------------------- StackNode -----------------------
int StackNode::numStackNodesAllocd=0;
int StackNode::maxStackNodesAllocd=0;


StackNode::StackNode()
  : state(STATE_INVALID),
    leftSiblings(),
    firstSib(NULL, NULL_SVAL  SOURCELOCARG( SL_UNKNOWN ) ),
    referenceCount(0),
    determinDepth(0),
    glr(NULL)
{
  // the interesting stuff happens in init()
}

StackNode::~StackNode()
{
  // the interesting stuff happens in deinit()
}


inline void StackNode::init(StateId st, GLR *g)
{
  state = st;
  xassertdb(leftSiblings.isEmpty());
  xassertdb(hasZeroSiblings());
  referenceCount = 0;
  determinDepth = 1;    // 0 siblings now, so this node is unambiguous
  glr = g;

  #if DO_ACCOUNTING
    INC_HIGH_WATER(numStackNodesAllocd, maxStackNodesAllocd);
    //TRACE("nodes", "(!!!) init stack node: num=" << numStackNodesAllocd
    //            << ", max=" << maxStackNodesAllocd);
  #endif
}

inline void StackNode::decrementAllocCounter()
{
  #if DO_ACCOUNTING
    numStackNodesAllocd--;
    //TRACE("nodes", "(...) deinit stack node: num=" << numStackNodesAllocd
    //            << ", max=" << maxStackNodesAllocd);
  #endif
}

inline void StackNode::deinit()
{
  decrementAllocCounter();

  if (!unwinding()) {
    xassert(numStackNodesAllocd >= 0);
    xassert(referenceCount == 0);
  }

  deallocSemanticValues();

  // this is pulled out of 'deallocSemanticValues' since dSV gets
  // called from the mini-LR parser, which sets this to NULL itself
  // (and circumvents the refct decrement)
  firstSib.sib = NULL;
}

inline SymbolId StackNode::getSymbolC() const
{
  xassertdb((unsigned)state < (unsigned)(glr->tables->getNumStates()));
  return glr->tables->getStateSymbol(state);
}



void StackNode::deallocSemanticValues()
{
  // explicitly deallocate siblings, so I can deallocate their
  // semantic values if necessary (this requires knowing the
  // associated symbol, which the SiblingLinks don't know)
  if (firstSib.sib != NULL) {
    deallocateSemanticValue(getSymbolC(), glr->userAct, firstSib.sval);
  }

  while (leftSiblings.isNotEmpty()) {
    Owner<SiblingLink> sib(leftSiblings.removeAt(0));
    deallocateSemanticValue(getSymbolC(), glr->userAct, sib->sval);
  }
}


// add the very first sibling
inline void StackNode
  ::addFirstSiblingLink_noRefCt(StackNode *leftSib, SemanticValue sval
                                SOURCELOCARG( SourceLoc loc ) )
{
  xassertdb(hasZeroSiblings());

  // my depth will be my new sibling's depth, plus 1
  determinDepth = leftSib->determinDepth + 1;

  // we don't have any siblings yet; use embedded
  // don't update reference count of 'leftSib', instead caller must do so
  //firstSib.sib = leftSib;
  xassertdb(firstSib.sib == NULL);      // otherwise we'd miss a decRefCt
  firstSib.sib.setWithoutUpdateRefct(leftSib);

  firstSib.sval = sval;

  // initialize some other fields
  SOURCELOC( firstSib.loc = loc; )
  YIELD_COUNT( firstSib.yieldCount = 0; )
}


// add a new sibling by creating a new link
inline SiblingLink *StackNode::
  addSiblingLink(StackNode *leftSib, SemanticValue sval
                 SOURCELOCARG( SourceLoc loc ) )
{
  if (hasZeroSiblings()) {
    addFirstSiblingLink_noRefCt(leftSib, sval  SOURCELOCARG( loc ) );

    // manually increment leftSib's refct
    leftSib->incRefCt();

    // sibling link pointers are used to control the reduction
    // process in certain corner cases; an interior pointer
    // should work fine
    return &firstSib;
  }
  else {
    // as best I can tell, x86 static branch prediction is simply
    // "conditional forward branches are assumed not taken", hence
    // the uncommon case belongs in the 'else' branch
    return addAdditionalSiblingLink(leftSib, sval  SOURCELOCARG( loc ) );
  }
}


// pulled out of 'addSiblingLink' so I can inline addSiblingLink
// without excessive object code bloat; the branch represented by
// the code in this function is much less common
SiblingLink *StackNode::
  addAdditionalSiblingLink(StackNode *leftSib, SemanticValue sval
                           SOURCELOCARG( SourceLoc loc ) )
{
  // there's currently at least one sibling, and now we're adding another;
  // right now, no other stack node should point at this one (if it does,
  // most likely will catch that when we use the stale info)
  determinDepth = 0;

  SiblingLink *link = new SiblingLink(leftSib, sval  SOURCELOCARG( loc ) );
  leftSiblings.prepend(link);   // dsw: don't append; it becomes quadratic!
  return link;
}


// inlined for the GLR part; mini-LR doesn't use this directly;
// gcc will inline the first level, even though it's recursive,
// and the effect is significant (~10%) for GLR-only parser
inline void StackNode::decRefCt()
{
  xassert(referenceCount > 0);

  //printf("decrementing node %d to %d\n", state, referenceCount-1);

  if (--referenceCount == 0) {
    glr->stackNodePool->dealloc(this);
  }
}


SiblingLink const *StackNode::getUniqueLinkC() const
{
  xassert(hasOneSibling());
  return &firstSib;
}


SiblingLink *StackNode::getLinkTo(StackNode *another)
{
  // check first..
  if (firstSib.sib == another) {
    return &firstSib;
  }

  // check rest
  MUTATE_EACH_OBJLIST(SiblingLink, leftSiblings, sibIter) {
    SiblingLink *candidate = sibIter.data();
    if (candidate->sib == another) {
      return candidate;
    }
  }
  return NULL;
}


STATICDEF void StackNode::printAllocStats()
{
  std::cout << "stack nodes: " << numStackNodesAllocd
       << ", max stack nodes: " << maxStackNodesAllocd
       << std::endl;
}


int StackNode::computeDeterminDepth() const
{
  if (hasZeroSiblings()) {
    return 1;
  }
  else if (hasOneSibling()) {
    // it must be equal to sibling's, plus one
    return firstSib.sib->determinDepth + 1;
  }
  else {
    xassert(hasMultipleSiblings());
    return 0;
  }
}


// I sprinkle calls to this here and there; in NDEBUG mode
// they'll all disappear
inline void StackNode::checkLocalInvariants() const
{
  xassertdb(computeDeterminDepth() == determinDepth);
}


// ------------- stack node list ops ----------------
void decParserList(ArrayStack<StackNode*> &list)
{
  for (int i=0; i < list.length(); i++) {
    list[i]->decRefCt();
  }
}

void incParserList(ArrayStack<StackNode*> &list)
{
  for (int i=0; i < list.length(); i++) {
    list[i]->incRefCt();
  }
}

// candidate for adding to ArrayStack.. but I'm hesitant for some reason
bool parserListContains(ArrayStack<StackNode*> &list, StackNode *node)
{
  for (int i=0; i < list.length(); i++) {
    if (list[i] == node) {
      return true;
    }
  }
  return false;
}


// ------------------------- GLR ---------------------------
GLR::GLR(UserActions *user, ParseTables *t)
  : userAct(user),
    tables(t),
    lexerPtr(NULL),
    topmostParsers(),
    parserIndex(NULL),
    toPass(MAX_RHSLEN),
    prevTopmost(),
    stackNodePool(NULL),
    pathQueue(t),
    noisyFailedParse(true),
    trParse(tracingSys("parse")),
    trsParse(trace("parse") << "parse tracing enabled\n"),
    detShift(0),
    detReduce(0),
    nondetShift(0),
    nondetReduce(0),
    yieldThenMergeCt(0)
  // some fields (re-)initialized by 'clearAllStackNodes'
{
  // originally I had this inside glrParse() itself, but that
  // made it 25% slower!  gcc register allocator again!
  if (tracingSys("glrConfig")) {
    printConfig();
  }

  // the ordinary GLR core doesn't have this limitation because
  // it uses a growable array
  #if USE_MINI_LR
    // make sure none of the productions have right-hand sides
    // that are too long; I think it's worth doing an iteration
    // here since going over the limit would be really hard to
    // debug, and this ctor is of course outside the main
    // parsing loop
    for (int i=0; i < tables->getNumProds(); i++) {
      if (tables->getProdInfo(i).rhsLen > MAX_RHSLEN) {
        printf("Production %d contains %d right-hand side symbols,\n"
               "but the GLR core has been compiled with a limit of %d.\n"
               "Please adjust MAX_RHSLEN and recompile the GLR core.\n",
               i, tables->getProdInfo(i).rhsLen, MAX_RHSLEN);
        xfailure("cannot continue");
      }
    }
  #endif // USE_MINI_LR

  // check that the parse tables' compression (if any) is the same
  // as this core expects
  configCheck("EEF compression", ENABLE_EEF_COMPRESSION, tables->eef_enabled());
  configCheck("GCS compression", ENABLE_GCS_COMPRESSION, tables->gcs_enabled());
  configCheck("GCS column compression", ENABLE_GCS_COLUMN_COMPRESSION, tables->gcsc_enabled());
  configCheck("CRS compression", ENABLE_CRS_COMPRESSION, tables->crs_enabled());
}

void GLR::configCheck(char const *option, bool core, bool table)
{
  if (core != table) {
    xfailure(sm_stringc
      << "The GLR parser core was compiled with " << option
      << (core? " enabled" : " disabled")
      << ", but the parse tables generated by Elkhound have it "
      << (table? "enabled" : "disabled"));
  }
}

GLR::~GLR()
{
  if (parserIndex) {
    delete[] parserIndex;
  }

  // NOTE: must not delete 'tables' until after the 'decParserList'
  // calls above, because they refer to the tables!
}


void GLR::clearAllStackNodes()
{
  // the stack nodes themselves are now reference counted, so they
  // should already be cleared if we're between parses (modulo
  // creation of cycles, which I currently just ignore and allow to
  // leak..)
}


// print compile-time configuration; this is useful for making
// sure a given binary has been compiled the way you think
void GLR::printConfig() const
{
  printf("GLR configuration follows.  Settings marked with an\n"
         "asterisk (*) are the higher-performance settings.\n");

  printf("  source location information: \t\t\t%s\n",
         SOURCELOC(1+)0? "enabled" : "disabled *");

  printf("  stack node columns: \t\t\t\t%s\n",
         NODE_COLUMN(1+)0? "enabled" : "disabled *");

  printf("  semantic value yield count: \t\t\t%s\n",
         YIELD_COUNT(1+)0? "enabled" : "disabled *");

  printf("  ACTION_TRACE (for debugging): \t\t%s\n",
         ACTION(1+)0? "enabled" : "disabled *");

  printf("  NDEBUG: \t\t\t\t\t%s\n",
         IF_NDEBUG(1+)0? "set      *" : "not set");

  printf("  xassert-style assertions: \t\t\t%s\n",
         #ifdef NDEBUG_NO_ASSERTIONS
           "disabled *"
         #else
           "enabled"
         #endif
         );

  printf("  user actions: \t\t\t\t%s\n",
         USE_ACTIONS? "respected" : "ignored  *");

  printf("  token reclassification: \t\t\t%s\n",
         USE_RECLASSIFY? "enabled" : "disabled *");

  printf("  reduction cancellation: \t\t\t%s\n",
         USE_KEEP? "enabled" : "disabled *");

  printf("  mini-LR parser core: \t\t\t\t%s\n",
         USE_MINI_LR? "enabled  *" : "disabled");

  printf("  allocated-node and parse action accounting: \t%s\n",
         ACCOUNTING(1+)0? "enabled" : "disabled *");

  printf("  unrolled reduce loop: \t\t\t%s\n",
         USE_UNROLLED_REDUCE? "enabled  *" : "disabled");

  printf("  parser index: \t\t\t\t%s\n",
         #ifdef USE_PARSER_INDEX
           "enabled"
         #else
           "disabled *"
         #endif
         );

  // checking __OPTIMIZE__ is misleading if preprocessing is entirely
  // divorced from compilation proper, but I still think this printout
  // is useful; also, gcc does not provide a way to tell what level of
  // optimization was applied (as far as I know)
  printf("  C++ compiler's optimizer: \t\t\t%s\n",
         #ifdef __OPTIMIZE__
           "enabled  *"
         #else
           "disabled"
         #endif
         );

  // at the moment, disabling compression makes it fastest
  printf("  Error Entry Factoring (EEF): \t\t\t%s\n",
         ENABLE_EEF_COMPRESSION? "enabled" : "disabled *");
  printf("  Graph Coloring Scheme (GCS): \t\t\t%s\n",
         ENABLE_GCS_COMPRESSION? "enabled" : "disabled *");
  printf("  GCS for columns (GCSC): \t\t\t%s\n",
         ENABLE_GCS_COLUMN_COMPRESSION? "enabled" : "disabled *");
  printf("  Code Reduction Scheme (CRS): \t\t\t%s\n",
         ENABLE_CRS_COMPRESSION? "enabled" : "disabled *");
}


// used to extract the svals from the nodes just under the
// start symbol reduction
SemanticValue GLR::grabTopSval(StackNode *node)
{
  SiblingLink *sib = node->getUniqueLink();
  SemanticValue ret = sib->sval;
  sib->sval = duplicateSemanticValue(node->getSymbolC(), sib->sval);

  TRSACTION("dup'd " << ret << " for top sval, yielded " << sib->sval);

  return ret;
}


// This macro has been pulled out so I can have even finer control
// over the allocation process from the mini-LR core.
//   dest: variable into which the pointer to the new node will be put
//   state: DFA state for this node
//   glr: pointer to the associated GLR object
//   pool: node pool from which to allocate
#define MAKE_STACK_NODE(dest, state, glr, pool)              \
  dest = (pool).alloc();                                     \
  dest->init(state, glr);                                    \
  NODE_COLUMN( dest->column = (glr)->globalNodeColumn; )

// more-friendly inline version, for use outside mini-LR
inline StackNode *GLR::makeStackNode(StateId state)
{
  StackNode *sn;
  MAKE_STACK_NODE(sn, state, this, *stackNodePool);
  return sn;
}


// add a new parser to the 'topmostParsers' list, maintaing
// related invariants
inline void GLR::addTopmostParser(StackNode *parser)
{
  parser->checkLocalInvariants();

  topmostParsers.push(parser);
  parser->incRefCt();

  // I implemented this index, and then discovered it made no difference
  // (actually, slight degradation) in performance; so for now it will
  // be an optional design choice, off by default
  #ifdef USE_PARSER_INDEX
    // fill in the state id index; if the assertion here ever fails, it
    // means there are more than 255 active parsers; either the grammer
    // is highly ambiguous by mistake, or else ParserIndexEntry needs to
    // be re-typedef'd to something bigger than 'char'
    int index = topmostParsers.length()-1;   // index just used
    xassert(index < INDEX_NO_PARSER);

    xassert(parserIndex[parser->state] == INDEX_NO_PARSER);
    parserIndex[parser->state] = index;
  #endif // USE_PARSER_INDEX
}


void GLR::buildParserIndex()
{
  if (parserIndex) {
    delete[] parserIndex;
  }
  parserIndex = new ParserIndexEntry[tables->getNumStates()];
  {
    for (int i=0; i < tables->getNumStates(); i++) {
      parserIndex[i] = INDEX_NO_PARSER;
    }
  }
}


bool GLR::glrParse(LexerInterface &lexer, SemanticValue &treeTop)
{
  #if !ACTION_TRACE
    // tell the user why "-tr action" doesn't do anything, if
    // they specified that
    trace("action") << "warning: ACTION_TRACE is currently disabled by a\n";
    trace("action") << "compile-time switch, so you won't see parser actions.\n";
  #endif

  #ifdef NDEBUG
    trace("parse") << "warning: Because NDEBUG was specified when elkhound was\n";
    trace("parse") << "         compiled, the 'parse' tracing flag does nothing.\n";
  #endif

  // get ready..
  traceProgress(2) << "parsing...\n";
  clearAllStackNodes();

  // this should be reset to NULL on all exit paths..
  lexerPtr = &lexer;

  // build the parser index (I do this regardless of whether I'm going
  // to use it, because up here it makes no performance difference,
  // and I'd like as little code as possible being #ifdef'd)
  buildParserIndex();

  // call the inner parser core, which is a static member function
  bool ret = innerGlrParse(*this, lexer, treeTop);
  stackNodePool = NULL;     // prevent dangling references
  if (!ret) {
    lexerPtr = NULL;
    return ret;
  }

  // sm: I like to always see these statistics, but dsw doesn't,
  // so I'll just set ELKHOUND_DEBUG in my .bashrc
  if (getenv("ELKHOUND_DEBUG")) {
    #if DO_ACCOUNTING
      StackNode::printAllocStats();
      std::cout << "detShift=" << detShift
           << ", detReduce=" << detReduce
           << ", nondetShift=" << nondetShift
           << ", nondetReduce=" << nondetReduce
           << std::endl;
      //PVAL(parserMerges);
      PVAL(computeDepthIters);

      PVAL(yieldThenMergeCt);
      PVAL(totalExtracts);
      PVAL(multipleDelayedExtracts);
    #endif
  }

  lexerPtr = NULL;
  return ret;
}


// old note: this function's complexity and/or size is *right* at the
// limit of what gcc-2.95.3 is capable of optimizing well; I've already
// pulled quite a bit of functionality into separate functions to try
// to reduce the register pressure, but it's still near the limit;
// if you do something to cross a pressure threshold, performance drops
// 25% so watch out!
//
// This function is the core of the parser, and its performance is
// critical to the end-to-end performance of the whole system.  It is
// a static member so the accesses to 'glr' (aka 'this') will be
// visible.
STATICDEF bool GLR
  ::innerGlrParse(GLR &glr, LexerInterface &lexer, SemanticValue &treeTop)
{
  #ifndef NDEBUG
    bool doDumpGSS = tracingSys("dumpGSS");
  #endif

  // pull a bunch of things out of 'glr' so they'll be accessible from
  // the stack frame instead of having to indirect into the 'glr' object
  UserActions *userAct = glr.userAct;
  ParseTables *tables = glr.tables;
  #if USE_MINI_LR
    ArrayStack<StackNode*> &topmostParsers = glr.topmostParsers;
  #endif

  // lexer token function
  LexerInterface::NextTokenFunc nextToken = lexer.getTokenFunc();

  #if USE_RECLASSIFY
  // reclassifier
  UserActions::ReclassifyFunc reclassifyToken =
    userAct->getReclassifier();
  #endif

  // the stack node pool is a local variable of this function for
  // fastest access by the mini-LR core; other parts of the algorihthm
  // can access it using a pointer stored in the GLR class (caller
  // nullifies this pointer afterward to prevent dangling references)
  ObjectPool<StackNode> stackNodePool(30);
  glr.stackNodePool = &stackNodePool;

  // create an initial ParseTop with grammar-initial-state,
  // set active-parsers to contain just this
  NODE_COLUMN( glr.globalNodeColumn = 0; )
  {
    StackNode *first = glr.makeStackNode(tables->startState);
    glr.addTopmostParser(first);
  }

  #if USE_MINI_LR
    // reduction action function
    UserActions::ReductionActionFunc reductionAction =
      userAct->getReductionAction();

    // this is *not* a reference to the 'glr' member because it
    // doesn't need to be shared with the rest of the algorithm (it's
    // only used in the Mini-LR core), and by having it directly on
    // the stack another indirection is saved
    //
    // new approach: let's try embedding this directly into the stack
    // (this saves 10% in end-to-end performance!)
    //GrowArray<SemanticValue> toPass(TYPICAL_MAX_RHSLEN);
    SemanticValue toPass[MAX_RHSLEN];
  #endif

  // count # of times we use mini LR
  ACCOUNTING( int localDetShift=0; int localDetReduce=0; )

  // for each input symbol
  #ifndef NDEBUG
    int tokenNumber = 0;

    // some debugging streams so the TRSPARSE etc. macros work
    bool trParse       = glr.trParse;
    std::ostream &trsParse  = glr.trsParse;
  #endif
  for (;;) {
    // debugging
    TRSPARSE(
           "------- "
        << "processing token " << lexer.tokenDesc()
        << ", " << glr.topmostParsers.length() << " active parsers"
        << " -------"
    )
    TRSPARSE("Stack:" << glr.stackSummary())

    #ifndef NDEBUG
      if (doDumpGSS) {
        glr.dumpGSS(tokenNumber);
      }
    #endif

    // get token type, possibly using token reclassification
    #if USE_RECLASSIFY
      lexer.type = reclassifyToken(userAct, lexer.type, lexer.sval);
    #else     // this is what bccgr does
      //if (lexer.type == 1 /*L2_NAME*/) {
      //  lexer.type = 3 /*L2_VARIABLE_NAME*/;
      //}
    #endif

    // alternate debugging; print after reclassification
    TRSACTION("lookahead token: " << lexer.tokenDesc() <<
              " aka " << userAct->terminalDescription(lexer.type, lexer.sval));

  #if USE_MINI_LR
    // try to cache a few values in locals (this didn't help any..)
    //ActionEntry const * const actionTable = this->tables->actionTable;
    //int const numTerms = this->tables->numTerms;

  tryDeterministic:
    // --------------------- mini-LR parser -------------------------
    // optimization: if there's only one active parser, and the
    // action is unambiguous, and it doesn't involve traversing
    // parts of the stack which are nondeterministic, then do the
    // parse action the way an ordinary LR parser would
    //
    // please note:  The code in this section is cobbled together
    // from various other GLR functions.  Everything here appears in
    // at least one other place, so modifications will usually have
    // to be done in both places.
    //
    // This code is the core of the parsing algorithm, so it's a bit
    // hairy for its performance optimizations.
    if (topmostParsers.length() == 1) {
      StackNode *parser = topmostParsers[0];
      xassertdb(parser->referenceCount==1);     // 'topmostParsers[0]' is referrer

      #if ENABLE_EEF_COMPRESSION
        if (tables->actionEntryIsError(parser->state, lexer.type)) {
          return false;    // parse error
        }
      #endif

      ActionEntry action =
        tables->getActionEntry_noError(parser->state, lexer.type);

      // I decode reductions before shifts because:
      //   - they are 4x more common in my C grammar
      //   - decoding a reduction is one less integer comparison
      // however I can only measure ~1% performance difference
      if (tables->isReduceAction(action)) {
        ACCOUNTING( localDetReduce++; )
        int prodIndex = tables->decodeReduce(action, parser->state);
        ParseTables::ProdInfo const &prodInfo = tables->getProdInfo(prodIndex);
        int rhsLen = prodInfo.rhsLen;
        if (rhsLen <= parser->determinDepth) {
          // can reduce unambiguously

          // I need to hide this declaration when debugging is off and
          // optimizer and -Werror are on, because it provokes a warning
          TRSPARSE_DECL( int startStateId = parser->state; )

          // if we're tracing actions, I'm going to build a sm_string
          // that describes all of the RHS symbols
          ACTION(
            sm_string rhsDescription("");
            if (rhsLen == 0) {
              // print something anyway
              rhsDescription = " empty";
            }
          )

          // record location of left edge; defaults to no location
          // (used for epsilon rules)
          // update: use location of lookahead token instead, for epsilons
          SOURCELOC( SourceLoc leftEdge = lexer.loc; )

          //toPass.ensureIndexDoubler(rhsLen-1);
          xassertdb(rhsLen <= MAX_RHSLEN);

          // we will manually sm_string the stack nodes together onto
          // the free list in 'stackNodePool', and 'prev' will point
          // to the head of the current list; at the end, we'll
          // install the final value of 'prev' back into
          // 'stackNodePool' as the new head of the list
          StackNode *prev = stackNodePool.private_getHead();

          #if USE_UNROLLED_REDUCE
            // What follows is unrollings of the loop below,
            // labeled "loop for arbitrary rhsLen".  Read that loop
            // before the unrollings here, since I omit the comments
            // here.  In general, this program should be correct
            // whether USE_UNROLLED_REDUCE is set or not.
            //
            // To produce the unrolled versions, simply copy all of the
            // noncomment lines from the general loop, and replace the
            // occurrence of 'i' with the value of one less than the 'case'
            // label number.
            switch ((unsigned)rhsLen) {    // gcc produces slightly better code if I cast to unsigned first
              case 1: {
                SiblingLink &sib = parser->firstSib;
                toPass[0] = sib.sval;
                ACTION( rhsDescription =
                  sm_stringc << " "
                          << symbolDescription(parser->getSymbolC(), userAct, sib.sval)
                          << rhsDescription; )
                SOURCELOC(
                  if (sib.validLoc()) {
                    leftEdge = sib.loc;
                  }
                )
                parser->nextInFreeList = prev;
                prev = parser;
                parser = sib.sib;
                xassertdb(parser->referenceCount==1);
                xassertdb(prev->referenceCount==1);
                prev->decrementAllocCounter();
                prev->firstSib.sib.setWithoutUpdateRefct(NULL);
                xassertdb(parser->referenceCount==1);
                // drop through into next case
              }

              case 0:
                // nothing to do
                goto afterGeneralLoop;
            }
          #endif // USE_UNROLLED_REDUCE

          // ------ loop for arbitrary rhsLen ------
          // pop off 'rhsLen' stack nodes, collecting as many semantic
          // values into 'toPass'
          // NOTE: this loop is the innermost inner loop of the entire
          // parser engine -- even *one* branch inside the loop body
          // costs about 30% end-to-end performance loss!
          for (int i = rhsLen-1; i >= 0; i--) {
            // grab 'parser's only sibling link
            //SiblingLink *sib = parser->getUniqueLink();
            SiblingLink &sib = parser->firstSib;

            // Store its semantic value it into array that will be
            // passed to user's routine.  Note that there is no need to
            // dup() this value, since it will never be passed to
            // another action routine (avoiding that overhead is
            // another advantage to the LR mode).
            toPass[i] = sib.sval;

            // when tracing actions, continue building rhs desc
            ACTION( rhsDescription =
              sm_stringc << " "
                      << symbolDescription(parser->getSymbolC(), userAct, sib.sval)
                      << rhsDescription; )

            // not necessary:
            //   sib.sval = NULL;                  // link no longer owns the value
            // this assignment isn't necessary because the usual treatment
            // of NULL is to ignore it, and I manually ignore *any* value
            // in the inline-expanded code below

            // if it has a valid source location, grab it
            SOURCELOC(
              if (sib.validLoc()) {
                leftEdge = sib.loc;
              }
            )

            // pop 'parser' and move to the next one
            parser->nextInFreeList = prev;
            prev = parser;
            parser = sib.sib;

            // don't actually increment, since I now no longer actually decrement
            // cancelled(1) effect: parser->incRefCt();    // so 'parser' survives deallocation of 'sib'
            // cancelled(1) observable: xassertdb(parser->referenceCount==1);       // 'sib' and the fake one

            // so now it's just the one
            xassertdb(parser->referenceCount==1);     // just 'sib'

            xassertdb(prev->referenceCount==1);
            // expand "prev->decRefCt();"             // deinit 'prev', dealloc 'sib'
            {
              // I don't actually decrement the reference count on 'prev'
              // because it will be reset to 0 anyway when it is inited
              // the next time it is used
              //prev->referenceCount = 0;

              // adjust the global count of stack nodes
              prev->decrementAllocCounter();

              // I previously had a test for "prev->firstSib.sval != NULL",
              // but that can't happen because I set it to NULL above!
              // (as the alias sib.sval)
              // update: now I don't even set it to NULL because the code here
              // has been changed to ignore *any* value
              //if (prev->firstSib.sval != NULL) {
              //  std::cout << "I GOT THE ANALYSIS WRONG!\n";
              //}

              // cancelled(1) effect: parser->decRefCt();
              prev->firstSib.sib.setWithoutUpdateRefct(NULL);

              // possible optimization: I could eliminiate
              // "prev->firstSib.sib=NULL" if I consistently modified all
              // creation of stack nodes to treat sib as a dead value:
              // right after creation I would make sure the new
              // sibling value *overwrites* sib, and no attempt is
              // made to decrement a refct on the dead value

              // this is obviated by the manual construction of the
              // free list links (nestInFreeList) above
              //stackNodePool.deallocNoDeinit(prev);
            }

            xassertdb(parser->referenceCount==1);     // fake refct only
          } // end of general rhsLen loop

        #if USE_UNROLLED_REDUCE    // suppress the warning when not using it..
        afterGeneralLoop:
        #endif
          // having now manually strung the deallocated stack nodes together
          // on the free list, I need to make the node pool's head point at them
          stackNodePool.private_setHead(prev);

          // call the user's action function (TREEBUILD)
          SemanticValue sval =
          #if USE_ACTIONS
            reductionAction(userAct, prodIndex, toPass /*.getArray()*/
                            SOURCELOCARG( leftEdge ) );
          #else
            NULL;
          #endif

          // now, push a new state; essentially, shift prodInfo.lhsIndex.
          // do "glrShiftNonterminal(parser, prodInfo.lhsIndex, sval, leftEdge);",
          // except avoid interacting with the worklists

          // this is like a shift -- we need to know where to go; the
          // 'goto' table has this information
          StateId newState = tables->decodeGoto(
            tables->getGotoEntry(parser->state, prodInfo.lhsIndex),
            prodInfo.lhsIndex);

          // debugging
          TRSPARSE("state " << startStateId <<
                   ", (unambig) reduce by " << prodIndex <<
                   " (len=" << rhsLen <<
                   "), back to " << parser->state <<
                   " then out to " << newState);

          // 'parser' has refct 1, reflecting the local variable only
          xassertdb(parser->referenceCount==1);

          // push new state
          StackNode *newNode;
          MAKE_STACK_NODE(newNode, newState, &glr, stackNodePool)

          newNode->addFirstSiblingLink_noRefCt(
            parser, sval  SOURCELOCARG( leftEdge ) );
          // cancelled(3) effect: parser->incRefCt();

          // cancelled(3) effect: xassertdb(parser->referenceCount==2);
          // expand:
          //   "parser->decRefCt();"                 // local variable "parser" about to go out of scope
          {
            // cancelled(3) effect: parser->referenceCount = 1;
          }
          xassertdb(parser->referenceCount==1);

          // replace whatever is in 'topmostParsers[0]' with 'newNode'
          topmostParsers[0] = newNode;
          newNode->incRefCt();
          xassertdb(newNode->referenceCount == 1);   // topmostParsers[0] is referrer

          // emit some trace output
          TRSACTION("  " <<
                    symbolDescription(newNode->getSymbolC(), userAct, sval) <<
                    " ->" << rhsDescription);

          #if USE_KEEP
            // see if the user wants to keep this reduction
            if (!userAct->keepNontermValue(prodInfo.lhsIndex, sval)) {
              ACTION( sm_string lhsDesc =
                        userAct->nonterminalDescription(prodInfo.lhsIndex, sval); )
              TRSACTION("    CANCELLED " << lhsDesc);
              glr.printParseErrorMessage(newNode->state);
              ACCOUNTING(
                glr.detShift += localDetShift;
                glr.detReduce += localDetReduce;
              )

              // TODO: I'm pretty sure I'm not properly cleaning
              // up all of my state here..
              return false;
            }
          #endif // USE_KEEP

          // after all this, we haven't shifted any tokens, so the token
          // context remains; let's go back and try to keep acting
          // determinstically (if at some point we can't be deterministic,
          // then we drop into full GLR, which always ends by shifting)
          goto tryDeterministic;
        }
      }

      else if (tables->isShiftAction(action)) {
        ACCOUNTING( localDetShift++; )

        // can shift unambiguously
        StateId newState = tables->decodeShift(action, lexer.type);

        TRSPARSE("state " << parser->state <<
                 ", (unambig) shift token " << lexer.tokenDesc() <<
                 ", to state " << newState);

        NODE_COLUMN( glr.globalNodeColumn++; )

        StackNode *rightSibling;
        MAKE_STACK_NODE(rightSibling, newState, &glr, stackNodePool);

        rightSibling->addFirstSiblingLink_noRefCt(
          parser, lexer.sval  SOURCELOCARG( lexer.loc ) );
        // cancelled(2) effect: parser->incRefCt();

        // replace 'parser' with 'rightSibling' in the topmostParsers list
        topmostParsers[0] = rightSibling;
        // cancelled(2) effect: xassertdb(parser->referenceCount==2);         // rightSibling & topmostParsers[0]
        // expand "parser->decRefCt();"
        {
          // cancelled(2) effect: parser->referenceCount = 1;
        }
        xassertdb(parser->referenceCount==1);         // rightSibling

        xassertdb(rightSibling->referenceCount==0);   // just created
        // expand "rightSibling->incRefCt();"
        {
          rightSibling->referenceCount = 1;
        }
        xassertdb(rightSibling->referenceCount==1);   // topmostParsers[0] refers to it

        // get next token
        goto getNextToken;
      }

      else {
        // error or ambig; not deterministic
      }
    }
    // ------------------ end of mini-LR parser ------------------
  #endif // USE_MINI_LR

    // if we get here, we're dropping into the nondeterministic GLR
    // algorithm in its full glory
    if (!glr.nondeterministicParseToken()) {
      return false;
    }

  #if USE_MINI_LR    // silence a warning when it's not enabled
  getNextToken:
  #endif
    // was that the last token?
    if (lexer.type == 0) {
      break;
    }

    // get the next token
    nextToken(&lexer);
    #ifndef NDEBUG
      tokenNumber++;
    #endif
  }

  // push stats into main object
  ACCOUNTING(
    glr.detShift += localDetShift;
    glr.detReduce += localDetReduce;
  )

  // end of parse; note that this function must be called *before*
  // the stackNodePool is deallocated
  return glr.cleanupAfterParse(treeTop);
}


// diagnostic/debugging function: yield sequence of
// states represented by 'parser'; in the case of
// ambiguity, just show one...
sm_string stackTraceString(StackNode *parser)
{
  // hmm.. what to do about cyclic stacks?
  return sm_string("need to think about this some more..");
}


// return false if caller should return false; pulled out of
// glrParse to reduce register pressure (but didn't help as
// far as I can tell!)
bool GLR::nondeterministicParseToken()
{
  //std::cout << "not deterministic\n";

  // ([GLR] called the code from here to the end of
  // the loop 'parseword')

  // work through the worklist
  StateId lastToDie = STATE_INVALID;

  // do all reduction explicitly first, then all shifts by
  // re-iterating over topmost parsers
  int i;
  for (i=0; i < topmostParsers.length(); i++) {
    StackNode *parser = topmostParsers[i];

    ActionEntry action =
      tables->getActionEntry(parser->state, lexerPtr->type);
    int actions = rwlEnqueueReductions(parser, action, NULL /*sibLink*/);

    if (actions == 0) {
      TRSPARSE("parser in state " << parser->state << " died");
      lastToDie = parser->state;
    }
  }

  // now that the reductions for all the existing topmost states
  // have been enqueued, process that worklist
  rwlProcessWorklist();

  // finally, do all the shifts that the topmost states can do
  rwlShiftTerminals();


  // if all active parsers have died, there was an error
  if (topmostParsers.isEmpty()) {
    printParseErrorMessage(lastToDie);
    return false;
  }
  else {
    return true;
  }
}


// pulled out of glrParse() to reduce register pressure
void GLR::printParseErrorMessage(StateId lastToDie)
{
  if (!noisyFailedParse) {
    return;
  }

  // print which tokens could have allowed progress; this isn't
  // perfect because I'm only printing this for one state, but in the
  // nondeterministic algorithm there might have been more than one
  // state that could have made progress..
  if (lastToDie != STATE_INVALID) {
    std::cout << "In state " << lastToDie << ", I expected one of these tokens:\n";
    std::cout << "  ";
    for (int i=0; i < tables->getNumTerms(); i++) {
      ActionEntry act = tables->getActionEntry(lastToDie, i);
      if (!tables->isErrorAction(act)) {
        //std::cout << "  [" << i << "] " << lexerPtr->tokenKindDesc(i) << "\n";
        std::cout << lexerPtr->tokenKindDesc(i) << ", ";
      }
    }
    std::cout << "\n";
  }
  else {
    // this happens because I lose the dead-parser info while processing
    // the reduction worklist; to implement this I'd need to remember each
    // state that died while processing the worklist; for now I'll just let
    // it be, and only have the right info sometimes
    std::cout << "(expected-token info not available due to nondeterministic mode)\n";
  }

  std::cout << toString(lexerPtr->loc)
       << ": Parse error (state " << lastToDie << ") at "
       << lexerPtr->tokenDesc()
       << std::endl;

  // removing this for now since keeping it would mean putting
  // sample inputs and left contexts for all states into the
  // parse tables
  #if 0
  if (lastToDie == STATE_INVALID) {
    // I'm not entirely confident it has to be nonnull..
    std::cout << "what the?  lastToDie is STATE_INVALID??\n";
  }
  else {
    // print out the context of that parser
    std::cout << "last parser (state " << lastToDie << ") to die had:\n"
         << "  sample input: "
         << sampleInput(getItemSet(lastToDie)) << "\n"
         << "  left context: "
         << leftContextString(getItemSet(lastToDie)) << "\n";
  }
  #endif // 0
}


SemanticValue GLR::doReductionAction(
  int productionId, SemanticValue const *svals
  SOURCELOCARG( SourceLoc loc ) )
{
  // get the function pointer and invoke it; possible optimization
  // is to cache the function pointer in the GLR object
  return (userAct->getReductionAction())(userAct, productionId, svals  SOURCELOCARG(loc));
}


// pulled from glrParse() to reduce register pressure
bool GLR::cleanupAfterParse(SemanticValue &treeTop)
{
  traceProgress() << "done parsing\n";
  trsParse << "Parse succeeded!\n";


  // finish the parse by reducing to start symbol
  if (topmostParsers.length() != 1) {
    std::cout << "parsing finished with more than one active parser!\n";
    return false;
  }
  StackNode *last = topmostParsers.top();

  // pull out the semantic values; this assumes the start symbol
  // always looks like "Start -> Something EOF"; it also assumes
  // the top of the tree is unambiguous
  SemanticValue arr[2];
  StackNode *nextToLast = last->getUniqueLink()->sib;
  arr[0] = grabTopSval(nextToLast);   // Something's sval
  arr[1] = grabTopSval(last);         // eof's sval

  // reduce
  TRSACTION("handing toplevel sval " << arr[0] <<
            " and " << arr[1] <<
            " to top start's reducer");
  treeTop = doReductionAction(
              //getItemSet(last->state)->getFirstReduction()->prodIndex,
              tables->finalProductionIndex,
              arr
              SOURCELOCARG( last->getUniqueLinkC()->loc ) );

  // why do this song-and-dance here, instead of letting the normal
  // parser engine do the final reduction?  because the GLR algorithm
  // always finishes its iterations with a shift, and it's not trivial
  // to add a special exception for the case of the reduce which
  // finishes the parse

  // these also must be done before the pool goes away..
  decParserList(topmostParsers);

  return true;
}


// this used to be code in glrParse(), but its presense disturbs gcc's
// register allocator to the tune of a 33% performance hit!  so I've
// pulled it in hopes the allocator will be happier now
void GLR::pullFromTopmostParsers(StackNode *parser)
{
  int last = topmostParsers.length()-1;
  for (int i=0; i <= last; i++) {
    if (topmostParsers[i] == parser) {
      // remove it; if it's not last in the list, swap it with
      // the last one to maintain contiguity
      if (i < last) {
        topmostParsers[i] = topmostParsers[last];
        // (no need to actually copy 'i' into 'last')
      }
      topmostParsers.pop();     // removes a reference to 'parser'
      parser->decRefCt();       // so decrement reference count
      break;
    }
  }
}


// return true if the given parser can either shift or reduce.  NOTE:
// this isn't really sufficient for its intended purpose, since I
// don't check to see whether *further* actions after a reduce are
// possible; moreover, checking that could be very expensive, since
// there may be many paths along which to consider reducing, and many
// paths from that reduced node forward..
bool GLR::canMakeProgress(StackNode *parser)
{
  ActionEntry entry =
    tables->getActionEntry(parser->state, lexerPtr->type);

  return tables->isShiftAction(entry) ||
         tables->isReduceAction(entry) ||
         !tables->isErrorAction(entry);
}


// if an active parser is at 'state', return it; otherwise
// return NULL
StackNode *GLR::findTopmostParser(StateId state)
{
  #ifdef USE_PARSER_INDEX
    int index = parserIndex[state];
    if (index != INDEX_NO_PARSER) {
      return topmostParsers[index];
    }
    else {
      return NULL;
    }
  #else
    for (int i=0; i < topmostParsers.length(); i++) {
      StackNode *node = topmostParsers[i];
      if (node->state == state) {
        return node;
      }
    }
    return NULL;
  #endif
}


// print the graph-structured stack to a file, named according
// to the current token number, in a format suitable for a
// graph visualization tool of some sort
void GLR::dumpGSS(int tokenNumber) const
{
  FILE *dest = fopen(sm_stringc << "gss." << tokenNumber << ".g", "w");

  // list of nodes we've already printed, to avoid printing any
  // node more than once
  SObjList<StackNode> printed;

  // list of nodes to print; might intersect 'printed', in which case
  // such nodes should be discarded; initially contains all the active
  // parsers (tops of stacks)
  SObjList<StackNode> queue;
  for (int i=0; i < topmostParsers.length(); i++) {
    queue.append(topmostParsers[i]);
  }

  // keep printing nodes while there are still some to print
  while (queue.isNotEmpty()) {
    StackNode *node = queue.removeFirst();
    if (printed.contains(node)) {
      continue;
    }
    printed.append(node);

    // only edges actually get printed (since the node names
    // encode all the important information); so iterate over
    // the sibling links now; while iterating, add the discovered
    // nodes to the queue so we'll print them too
    if (node->firstSib.sib != NULL) {
      dumpGSSEdge(dest, node, node->firstSib.sib);
      queue.append(node->firstSib.sib);

      FOREACH_OBJLIST(SiblingLink, node->leftSiblings, iter) {
        dumpGSSEdge(dest, node, iter.data()->sib);
        queue.append(const_cast<StackNode*>( iter.data()->sib.getC() ));
      }
    }
  }

  fclose(dest);
}


void GLR::dumpGSSEdge(FILE *dest, StackNode const *src,
                                  StackNode const *target) const
{
  fprintf(dest, "e %d_%p_%d %d_%p_%d\n",
                0 NODE_COLUMN( + src->column ), src, src->state,
                0 NODE_COLUMN( + target->column ), target, target->state);
}


// alternative to above: stack info in a single sm_string
sm_string GLR::stackSummary() const
{
  sm_stringBuilder sb;

  // list of nodes we've already printed, to avoid printing any
  // node more than once
  SObjList<StackNode const> printed;

  for (int i=0; i < topmostParsers.length(); i++) {
    sb << " (" << i << ": ";
    innerStackSummary(sb, printed, topmostParsers[i]);
    sb << ")";
  }

  return sb;
}

void GLR::nodeSummary(sm_stringBuilder &sb, StackNode const *node) const
{
  sb << node->state << "[" << node->referenceCount << "]";
}

void GLR::innerStackSummary(sm_stringBuilder &sb, SObjList<StackNode const> &printed,
                            StackNode const *node) const
{
  if (printed.contains(node)) {
    sb << "(rep:";
    nodeSummary(sb, node);
    sb << ")";
    return;
  }

  nodeSummary(sb, node);
  printed.append(node);

  if (!node->firstSib.sib) {
    return;   // no siblings
  }

  sb << "-";

  if (node->leftSiblings.isEmpty()) {
    // one sibling
    innerStackSummary(sb, printed, node->firstSib.sib);
  }
  else {
    // multiple siblings
    sb << "(";
    innerStackSummary(sb, printed, node->firstSib.sib);

    FOREACH_OBJLIST(SiblingLink, node->leftSiblings, iter) {
      sb << "|";
      innerStackSummary(sb, printed, iter.data()->sib);
    }
    sb << ")";
  }
}


#if 0
SemanticValue GLR::getParseResult()
{
  // the final topmost parser is the one that shifted the
  // end-of-stream marker, so we want its left sibling, since that
  // will be the reduction(s) to the start symbol
  SemanticValue sv =
    topmostParsers.first()->                    // parser that shifted end-of-stream
      leftSiblings.first()->sib->              // parser that shifted start symbol
      leftSiblings.first()->                   // sibling link with start symbol
      sval;                                    // start symbol tree node

  return sv;
}
#endif // 0


// -------------- reduction worklist (RWL) algorithm --------------
// This algorithm is an attempt to avoid the problem where a semantic
// value is yielded to a reduction action, but then merged with
// another semantic value, such that the original one yielded is now
// stale.  It's described in more detail in the tech report.

ReductionPathQueue::Path::Path()
  : startStateId(STATE_INVALID),
    prodIndex(-1),
    startColumn(-1),
    leftEdgeNode(NULL),
    sibLinks(INITIAL_RHSLEN_SIZE),
    symbols(INITIAL_RHSLEN_SIZE)
{
  next = NULL;
}

ReductionPathQueue::Path::~Path()
{}


void ReductionPathQueue::Path::init(StateId ssi, int pi, int rhsLen)
{
  startStateId = ssi;
  prodIndex = pi;

  sibLinks.ensureIndexDoubler(rhsLen);
  symbols.ensureIndexDoubler(rhsLen);
}


ReductionPathQueue::ReductionPathQueue(ParseTables *t)
  : top(NULL),
    pathPool(30),    // arbitrary initial pool size
    tables(t)
{}

ReductionPathQueue::~ReductionPathQueue()
{
  // 'pathPool' will automatically array-deallocate all of the
  // paths, which will themselves then delete their internal
  // 'sibLinks' and 'symbols' arrays
}


ReductionPathQueue::Path *ReductionPathQueue::newPath(
  StateId startStateId, int prodIndex, int rhsLen)
{
  Path *p = pathPool.alloc();
  p->init(startStateId, prodIndex, rhsLen);
  return p;
}


void ReductionPathQueue::insertPathCopy(Path const *src, StackNode *leftEdge)
{
  ParseTables::ProdInfo const &prodInfo = tables->getProdInfo(src->prodIndex);

  // make a new node
  Path *p = pathPool.alloc();
  p->init(src->startStateId, src->prodIndex, prodInfo.rhsLen);

  // fill in left edge info
  p->leftEdgeNode = leftEdge;
  p->startColumn = leftEdge->column;

  // copy the path info
  for (int i = prodInfo.rhsLen-1; i>=0; i--) {
    p->sibLinks[i] = src->sibLinks[i];
    p->symbols[i] = src->symbols[i];
  }

  // find the proper place to insert it
  if (!top || goesBefore(p, top)) {
    // prepend
    p->next = top;
    top = p;
  }
  else {
    // search
    Path *prev = top;
    while (prev->next && !goesBefore(p, prev->next)) {
      prev = prev->next;
    }

    // insert
    p->next = prev->next;
    prev->next = p;
  }
}

bool ReductionPathQueue::goesBefore(Path const *p1, Path const *p2) const
{
  if (p1->startColumn > p2->startColumn) {
    // 'p1' spans fewer tokens, so it goes first
    return true;
  }
  else if (p2->startColumn > p1->startColumn) {
    // same logic
    return false;
  }
  else {
    // equal start columns, so compare ids of nonterminals
    // to which we're reducing in each case
    NtIndex p1NtIndex = tables->getProdInfo(p1->prodIndex).lhsIndex;
    NtIndex p2NtIndex = tables->getProdInfo(p2->prodIndex).lhsIndex;

    // consult total order on nonterminals
    int ord1 = tables->getNontermOrdinal(p1NtIndex);
    int ord2 = tables->getNontermOrdinal(p2NtIndex);

    return ord1 < ord2;
  }
}


inline ReductionPathQueue::Path *ReductionPathQueue::dequeue()
{
  Path *ret = top;
  top = top->next;
  return ret;
}


void ReductionPathQueue::deletePath(Path *p)
{
  pathPool.dealloc(p);
}


// process the reduction worklist
void GLR::rwlProcessWorklist()
{
  // location of this token
  SOURCELOC( SourceLoc tokenLoc = lexerPtr->loc; )

  while (pathQueue.isNotEmpty()) {
    // process the enabled reductions in priority order
    ReductionPathQueue::Path *path = pathQueue.dequeue();

    // info about the production
    ParseTables::ProdInfo const &prodInfo = tables->getProdInfo(path->prodIndex);
    int rhsLen = prodInfo.rhsLen;

    TRSPARSE("state " << path->startStateId <<
             ", reducing by production " << path->prodIndex <<
             " (rhsLen=" << rhsLen <<
             "), back to state " << path->leftEdgeNode->state);

    ACCOUNTING( nondetReduce++; )

    // record location of left edge; initially is location of
    // the lookahead token
    SOURCELOC( SourceLoc leftEdge = tokenLoc; )

    // build description of rhs for tracing
    ACTION(
      sm_string rhsDescription("");
      if (rhsLen == 0) {
        // print something anyway
        rhsDescription = " empty";
      }
    )

    // before calling the user, duplicate any needed values; this loop
    // goes from right to left backwards so that 'leftEdge' is
    // computed properly
    toPass.ensureIndexDoubler(rhsLen-1);
    for (int i=rhsLen-1; i >= 0; i--) {
      SiblingLink *sib = path->sibLinks[i];

      // we're about to yield sib's 'sval' to the reduction action
      toPass[i] = sib->sval;

      // continue building rhs desc
      ACTION( rhsDescription =
        sm_stringc << symbolDescription(path->symbols[i], userAct, sib->sval)
                << " "
                << rhsDescription;
      )

      // left edge?  or, have all previous tokens failed to yield
      // information?
      SOURCELOC(
        if (sib->loc != SL_UNKNOWN) {
          leftEdge = sib->loc;
        }
      )

      // we inform the user, and the user responds with a value
      // to be kept in this sibling link *instead* of the passed
      // value; if this link yields a value in the future, it will
      // be this replacement
      sib->sval = duplicateSemanticValue(path->symbols[i], sib->sval);

      YIELD_COUNT( sib->yieldCount++; )
    }

    // we've popped the required number of symbols; call the
    // user's code to synthesize a semantic value by combining them
    // (TREEBUILD)
    SemanticValue sval =
      doReductionAction(path->prodIndex, toPass.getArray()
                        SOURCELOCARG( leftEdge ) );

    // emit tracing diagnostics for this reduction
    ACTION( sm_string lhsDesc =
              userAct->nonterminalDescription(prodInfo.lhsIndex, sval); )
    TRSACTION("  " << lhsDesc << " ->" << rhsDescription);

    // see if the user wants to keep this reduction
    if (USE_KEEP &&
        !userAct->keepNontermValue(prodInfo.lhsIndex, sval)) {
      TRSACTION("    CANCELLED " << lhsDesc);
    }
    else {
      // shift the nonterminal with its reduced semantic value
      SiblingLink *newLink =
        rwlShiftNonterminal(path->leftEdgeNode, prodInfo.lhsIndex,
                            sval  SOURCELOCARG( leftEdge ) );

      if (newLink) {
        // for each 'finished' parser ...
        for (int i=0; i < topmostParsers.length(); i++) {
          StackNode *parser = topmostParsers[i];

          // ... do any reduce actions that are now enabled by the new link
          ActionEntry action =
            tables->getActionEntry(parser->state, lexerPtr->type);
          rwlEnqueueReductions(parser, action, newLink);
        }
      }
    }

    pathQueue.deletePath(path);
  }
}


// shift reduction onto 'leftSibling' parser, 'lhsIndex' says which
// nonterminal is being shifted; 'sval' is the semantic value of this
// subtree, and 'loc' is the location of the left edge; return value
// is the newly added link, if one was added between existing nodes
// ([GLR] calls this function 'reducer')
//
// exactly one of three possible things happens:
//   - we make a new stack node
//   - we add a new link between existing stack nodes
//   - we merge two semantic values onto an existing link
SiblingLink *GLR::rwlShiftNonterminal(StackNode *leftSibling, int lhsIndex,
                                      SemanticValue /*owner*/ sval
                                      SOURCELOCARG( SourceLoc loc ) )
{
  // this is like a shift -- we need to know where to go; the
  // 'goto' table has this information
  StateId rightSiblingState = tables->decodeGoto(
    tables->getGotoEntry(leftSibling->state, lhsIndex), lhsIndex);

  // debugging
  TRSPARSE("state " << leftSibling->state <<
           ", shift nonterm " << lhsIndex <<
           ", to state " << rightSiblingState);

  // is there already an active parser with this state?
  StackNode *rightSibling = findTopmostParser(rightSiblingState);
  if (rightSibling) {
    // does it already have a sibling link to 'leftSibling'?
    SiblingLink *sibLink = rightSibling->getLinkTo(leftSibling);
    if (sibLink) {
      // we already have a sibling link, so we don't need to add one

      // +--------------------------------------------------+
      // | it is here that we are bringing the tops of two  |
      // | alternative parses together (TREEBUILD)          |
      // +--------------------------------------------------+

      // sometimes we are trying to merge dead trees--if the
      // 'rightSibling' cannot make progress at all, it would be much
      // better to just drop this alternative than demand the user
      // merge trees when there is not necessarily any ambiguity
      if (!canMakeProgress(rightSibling)) {
        // both trees are dead; deallocate one (the other alternative
        // will be dropped later, when 'rightSibling' is considered
        // for action in the usual way)
        TRSPARSE("avoided a merge by noticing the state was dead");
        deallocateSemanticValue(rightSibling->getSymbolC(), sval);
        return NULL;
      }

      // remember previous value, for yield count warning
      YIELD_COUNT(SemanticValue old2 = sibLink->sval);

      // remember descriptions of the values before they are merged
      ACTION(
        sm_string leftDesc = userAct->nonterminalDescription(lhsIndex, sibLink->sval);
        sm_string rightDesc = userAct->nonterminalDescription(lhsIndex, sval);
      )

      // call the user's code to merge, and replace what we have
      // now with the merged version
      sibLink->sval =
        userAct->mergeAlternativeParses(lhsIndex, sibLink->sval, sval  SOURCELOCARG( loc ) );

      // emit tracing diagnostics for the merge
      TRSACTION("  " <<
                userAct->nonterminalDescription(lhsIndex, sibLink->sval) <<
                " is MERGE of " << leftDesc << " and " << rightDesc);

      YIELD_COUNT(
        if (sibLink->yieldCount > 0) {
          // yield-then-merge (YTM) happened
          yieldThenMergeCt++;
          SOURCELOC( trace("ytm") << "at " << toString(loc) << std::endl; )

          // if merging yielded a new semantic value, then we most likely
          // have a problem; if it yielded the *same* value, then most
          // likely the user has implemented the 'ambiguity' link soln,
          // so we're ok
          if (old2 != sibLink->sval) {
            std::cout << "warning: incomplete parse forest: " << (void*)old2
                 << " has already been yielded, but it now has been "
                 << "merged with " << (void*)sval << " to make "
                 << (void*)(sibLink->sval) << " (lhsIndex="
                 << lhsIndex << ")" << std::endl;
          }
        }
      )

      // ok, done
      return NULL;

      // and since we didn't add a link, there is no potential for new
      // paths
    }

    // we get here if there is no suitable sibling link already
    // existing; so add the link (and keep the ptr for loop below)
    sibLink = rightSibling->addSiblingLink(leftSibling, sval  SOURCELOCARG( loc ) );

    // adding a new sibling link may have introduced additional
    // opportunties to do reductions from parsers we thought
    // we were finished with.
    //
    // what's more, it's not just the parser ('rightSibling') we
    // added the link to -- if rightSibling's itemSet contains 'A ->
    // alpha . B beta' and B ->* empty (so A's itemSet also has 'B
    // -> .'), then we reduced it (if lookahead ok), so
    // 'rightSibling' now has another left sibling with 'A -> alpha
    // B . beta'.  We need to let this sibling re-try its reductions
    // also.
    //
    // so, the strategy is to let all 'finished' parsers re-try
    // reductions, and process those that actually use the just-
    // added link

    // TODO: I think this code path is unusual; confirm by measurement
    // update: it's taken maybe 1 in 10 times through this function..
    parserMerges++;

    // we don't have to recompute if nothing else points at
    // 'rightSibling'; the refct is always at least 1 because we found
    // it on the "active parsers" worklist
    if (rightSibling->referenceCount > 1) {
      // since we added a new link *all* determinDepths might
      // be compromised; iterating more than once should be very
      // rare (and this code path should already be unusual)
      int changes=1, iters=0;
      while (changes) {
        changes = 0;
        for (int i=0; i < topmostParsers.length(); i++) {
          StackNode *parser = topmostParsers[i];
          int newDepth = parser->computeDeterminDepth();
          if (newDepth != parser->determinDepth) {
            changes++;
            parser->determinDepth = newDepth;
          }
        }
        iters++;
        xassert(iters < 1000);    // protect against infinite loop
        computeDepthIters++;
      }
    }

    // inform the caller that a new sibling link was added
    return sibLink;
  }

  else {
    // no, there is not already an active parser with this
    // state.  we must create one; it will become the right
    // sibling of 'leftSibling'
    rightSibling = makeStackNode(rightSiblingState);

    // add the sibling link (and keep ptr for tree stuff)
    rightSibling->addSiblingLink(leftSibling, sval  SOURCELOCARG( loc ) );

    // since this is a new parser top, it needs to become a
    // member of the frontier
    addTopmostParser(rightSibling);

    // here, rather than adding something to the parser worklist,
    // we'll directly expand its reduction paths and add them
    // to the reduction worklist
    ActionEntry action =
      tables->getActionEntry(rightSibling->state, lexerPtr->type);
    rwlEnqueueReductions(rightSibling, action, NULL /*sibLink*/);

    // no need for the elaborate re-checking above, since we
    // just created rightSibling, so no new opportunities
    // for reduction could have arisen
    return NULL;
  }
}


// find and enqueue all the reductions that 'parser' can do; 'action'
// is the parser's action code; we only consider reductions that use
// 'mustUseLink', if that is not NULL
//
// This function will enqueue reduction paths, ordered first by the
// number of terminals spanned and second by the nonterminal
// derivability relation on the nonterminal to which the path reduces
// (if A ->+ B then we will reduce to B before reducing to A, if
// terminal spans are equal).
//
// this function returns the # of actions the parser can take, as
// part of a rather weak error reporting scheme..
int GLR::rwlEnqueueReductions(StackNode *parser, ActionEntry action,
                              SiblingLink *mustUseLink)
{
  parser->checkLocalInvariants();

  if (tables->isShiftAction(action)) {
    // do nothing, we're only interested in reductions
    return 1;
  }
  else if (tables->isReduceAction(action)) {
    // reduce
    int prodIndex = tables->decodeReduce(action, parser->state);

    // get information about the production we'll use
    ParseTables::ProdInfo const &info = tables->getProdInfo(prodIndex);
    int rhsLen = info.rhsLen;
    xassert(rhsLen >= 0);    // paranoia before using this to control recursion

    // initialize a prototype Path which will monitor our progress
    // though the enumeration of all paths
    ReductionPathQueue::Path *proto =
      pathQueue.newPath(parser->state, prodIndex, rhsLen);

    // kick off the recursion
    rwlRecursiveEnqueue(proto, rhsLen, parser, mustUseLink);

    // deallocate the prototype
    pathQueue.deletePath(proto);

    return 1;
  }
  else if (tables->isErrorAction(action)) {
    // the parser dies, we don't do anything
    return 0;
  }
  else {
    // ambiguous; check for reductions
    ActionEntry *entry = tables->decodeAmbigAction(action, parser->state);
    for (int i=0; i<entry[0]; i++) {
      rwlEnqueueReductions(parser, entry[i+1], mustUseLink);
    }

    return entry[0];
  }
}


// arguments have same meanings as in 'rwlRecursiveEnqueue'
inline void GLR::rwlCollectPathLink(
  ReductionPathQueue::Path *proto, int popsRemaining,
  StackNode *currentNode, SiblingLink *mustUseLink, SiblingLink *linkToAdd)
{
  proto->sibLinks[popsRemaining] = linkToAdd;
  proto->symbols[popsRemaining] = currentNode->getSymbolC();

  if (linkToAdd == mustUseLink) {
    rwlRecursiveEnqueue(proto, popsRemaining, linkToAdd->sib,
                        NULL /*mustUseLink*/);
  }
  else {
    rwlRecursiveEnqueue(proto, popsRemaining, linkToAdd->sib,
                        mustUseLink);
  }
}

// recursive depth-first enumeration of paths
void GLR::rwlRecursiveEnqueue(
  ReductionPathQueue::Path *proto,  // prototype path, with path so far
  int popsRemaining,                // # of links yet to traverse to find a full path
  StackNode *currentNode,           // node we're at in the path
  SiblingLink *mustUseLink)         // link the path must use (if non-NULL)
{
  if (popsRemaining == 0) {
    // we found path of required length

    // if we have failed to use the required link, ignore this path
    if (mustUseLink != NULL) {
      return;
    }

    // the prototype path is the one we want; copy it, fill in
    // the 'startColumn', and insert it into the queue
    pathQueue.insertPathCopy(proto, currentNode);
  }

  else {
    // explore 'currentNode's siblings
    rwlCollectPathLink(proto, popsRemaining-1, currentNode, mustUseLink,
                       &(currentNode->firstSib));

    // test before dropping into the loop, since profiler reported
    // some time spent calling VoidListMutator::reset ..
    if (currentNode->leftSiblings.isNotEmpty()) {
      FOREACH_OBJLIST_NC(SiblingLink, currentNode->leftSiblings, sibling) {
        rwlCollectPathLink(proto, popsRemaining-1, currentNode, mustUseLink,
                           sibling.data());
      }
    }
  }
}


// final phase in processing of a token: all topmost parsers
// shift the current token, if they can
void GLR::rwlShiftTerminals()
{
  NODE_COLUMN( globalNodeColumn++; )

  // move all the parsers from 'topmostParsers' into 'prevTopmost'
  xassert(prevTopmost.isEmpty());
  prevTopmost.swapWith(topmostParsers);
  xassert(topmostParsers.isEmpty());

  // to solve the multi-yield problem for tokens, I'll remember
  // the previously-created sibling link (if any), and dup the
  // sval in that link as needed
  SiblingLink *prev = NULL;

  // foreach node in prevTopmost
  while (prevTopmost.isNotEmpty()) {
    // take the node from 'prevTopmost'; the refcount includes both
    // 'leftSibling' and 'prevTopmost', and then we decrement the
    // count to reflect that only 'leftSibling' has it
    RCPtr<StackNode> leftSibling(prevTopmost.pop());
    xassertdb(leftSibling->referenceCount >= 2);
    leftSibling->decRefCt();

    // where can this shift, if anyplace?
    ActionEntry action =
      tables->getActionEntry(leftSibling->state, lexerPtr->type);

    // we'll set this if we find a valid shift dest
    StateId newState = STATE_INVALID;

    // consult action table, looking only for shifts
    if (tables->isShiftAction(action)) {
      // unambiguous shift
      newState = tables->decodeShift(action, lexerPtr->type);
    }
    else if (tables->isReduceAction(action) ||
             tables->isErrorAction(action)) {
      // reduce or error
      continue;
    }
    else {
      // nondeterministic; get actions
      ActionEntry *entry = tables->decodeAmbigAction(action, leftSibling->state);

      // do each one
      for (int i=0; i<entry[0]; i++) {
        action = entry[i+1];
        if (tables->isShiftAction(action)) {
          // a shift was among the conflicted actions
          newState = tables->decodeShift(action, lexerPtr->type);
          break;
        }
      }

      // did we find a shift?
      if (newState == STATE_INVALID) {
        continue;    // no
      }
    }

    // found a shift to perform
    ACCOUNTING( nondetShift++; )

    // debugging
    TRSPARSE("state " << leftSibling->state <<
             ", shift token " << lexerPtr->tokenDesc() <<
             ", to state " << newState);

    // if there's already a parser with this state
    StackNode *rightSibling = findTopmostParser(newState);
    if (rightSibling != NULL) {
      // no need to create the node
    }

    else {
      // must make a new stack node
      rightSibling = makeStackNode(newState);

      // and add it to the active parsers
      addTopmostParser(rightSibling);
    }

    SemanticValue sval = lexerPtr->sval;
    if (prev) {
      // the 'sval' we just grabbed has already been claimed by
      // 'prev->sval'; get a fresh one by duplicating the latter
      sval = userAct->duplicateTerminalValue(lexerPtr->type, prev->sval);

      TRSACTION("  " << userAct->terminalDescription(lexerPtr->type, sval) <<
                " is (@lexer) DUP of " <<
                userAct->terminalDescription(lexerPtr->type, prev->sval));
    }

    // either way, add the sibling link now
    //TRSACTION("grabbed token sval " << lexerPtr->sval);
    prev = rightSibling->addSiblingLink(leftSibling, sval
                                        SOURCELOCARG( lexerPtr->loc ) );

    // adding this sibling link cannot violate the determinDepth
    // invariant of some other node, because all of the nodes created
    // or added-to during shifting do not have anything pointing at
    // them, so in particular nothing points to 'rightSibling'; a simple
    // check of this is to check the reference count and verify it is 1,
    // the 1 being for the 'topmostParsers' list it is on
    xassert(rightSibling->referenceCount == 1);
  }
}


// ------------------ stuff for outputting raw graphs ------------------
#if 0   // disabled for now
// name for graphs (can't have any spaces in the name)
sm_string stackNodeName(StackNode const *sn)
{
  Symbol const *s = sn->getSymbolC();
  char const *symName = (s? s->name.pcharc() : "(null)");
  return sm_stringb(sn->stackNodeId
              << ":col="  << sn->tokenColumn
              << ",st=" << sn->state->id
              << ",sym=" << symName);
}

// name for rules; 'rn' is the 'ruleNo'-th rule in 'sn'
// (again, no spaces allowed)
sm_string reductionName(StackNode const *sn, int ruleNo, Reduction const *red)
{
  return sm_stringb(sn->stackNodeId << "/" << ruleNo << ":"
              << replace(red->production->toString(), " ", "_"));
}


// this prints the graph in my java graph applet format, where
// nodes lines look like
//   n <name> <optional-desc>
// and edges look like
//   e <from> <to>
// unfortunately, the graph applet needs a bit of work before it
// is worthwhile to use this routinely (though it's great for
// quickly verifying a single (small) parse)
//
// however, it's worth noting that the text output is not entirely
// unreadable...
void GLR::writeParseGraph(char const *fname) const
{
  FILE *out = fopen(sm_stringb("graphs/" << fname), "w");
  if (!out) {
    xsyserror("fopen", sm_stringb("opening file `graphs/" << fname << "'"));
  }

  // header info
  fprintf(out, "# parse graph file: %s\n", fname);
  fprintf(out, "# automatically generated\n"
               "\n");

  #if 0    // can't do anymore because allStackNodes is gone ...
  // for each stack node
  FOREACH_OBJLIST(StackNode, allStackNodes, stackNodeIter) {
    StackNode const *stackNode = stackNodeIter.data();
    sm_string myName = stackNodeName(stackNode);

    // visual delimiter
    fputs(sm_stringb("\n# ------ node: " << myName << " ------\n"), out);

    // write info for the node itself
    fputs(sm_stringb("n " << myName << "\n\n"), out);

    // for all sibling links
    int ruleNo=0;
    FOREACH_OBJLIST(SiblingLink, stackNode->leftSiblings, sibIter) {
      SiblingLink const *link = sibIter.data();

      // write the sibling link
      fputs(sm_stringb("e " << myName << " "
                         << stackNodeName(link->sib) << "\n"), out);

      // ideally, we'd attach the reduction nodes directly to the
      // sibling edge.  however, since I haven't developed the
      // graph applet far enough for that, I'll instead attach it
      // to the stack node directly..

      if (link->treeNode->isNonterm()) {
        // for each reduction node
        FOREACH_OBJLIST(Reduction, link->treeNode->asNonterm().reductions,
                        redIter) {
          Reduction const *red = redIter.data();
          ruleNo++;

          sm_string ruleName = reductionName(stackNode, ruleNo, red);

          // write info for the rule node
          fputs(sm_stringb("n " << ruleName << "\n"), out);

          // put the link from the stack node to the rule node
          fputs(sm_stringb("e " << myName << " " << ruleName << "\n"), out);

          // write all child links
          // ACK!  until my graph format is better, this is almost impossible
          #if 0
          SFOREACH_OBJLIST(StackNode, rule->children, child) {
            fputs(sm_stringb("e " << ruleName << " "
                               << stackNodeName(child.data()) << "\n"), out);
          }
          #endif // 0

          // blank line for visual separation
          fputs("\n", out);
        } // for each reduction
      } // if is nonterminal
    } // for each sibling
  } // for each stack node
  #endif // 0

  // done
  if (fclose(out) != 0) {
    xsyserror("fclose");
  }
}
#endif // 0


// --------------------- testing ------------------------
// read an entire file into a single sm_string
// currenty is *not* pipe-frendly because it must seek
// (candidate for adding to 'str' module)
sm_string readFileIntoString(char const *fname)
{
  // open file
  FILE *fp = fopen(fname, "r");
  if (!fp) {
    xsyserror("fopen", sm_stringb("opening `" << fname << "' for reading"));
  }

  // determine file's length
  if (fseek(fp, 0, SEEK_END) < 0) {
    xsyserror("fseek");
  }
  int len = (int)ftell(fp);      // conceivably problematic cast..
  if (len < 0) {
    xsyserror("ftell");
  }
  if (fseek(fp, 0, SEEK_SET) < 0) {
    xsyserror("fseek");
  }

  // allocate a sufficiently large buffer
  sm_string ret(len);

  // read the file into that buffer
  if (fread(ret.pchar(), 1, len, fp) < (size_t)len) {
    xsyserror("fread");
  }

  // close file
  if (fclose(fp) < 0) {
    xsyserror("fclose");
  }

  // return the new sm_string
  return ret;
}


// EOF
