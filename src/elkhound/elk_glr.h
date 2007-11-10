// glr.h            see license.txt for copyright and terms of use
// GLR parsing algorithm

/*
 * Author: Scott McPeak, April 2000
 *
 * The fundamental concept in Generalized LR (GLR) parsing
 * is to permit (at least local) ambiguity by "forking" the
 * parse stack.  If the input is actually unambiguous, then
 * all but one of the forked parsers will, at some point,
 * fail to shift a symbol, and die.  If the input is truly
 * ambiguous, forked parsers rejoin at some point, and the
 * parse tree becomes a parse DAG, representing all possible
 * parses.  (In fact, since cyclic grammars are supported,
 * which can have an infinite number of parse trees for
 * some inputs, we may end up with a cyclic parse *graph*.)
 *
 * In the larger scheme of things, this level of support for
 * ambiguity is useful because it lets us use simpler and
 * more intuitive grammars, more sophisticated disambiguation
 * techniques, and parsing in the presence of incomplete
 * or incorrect information (e.g. in an editor).
 *
 * The downside is that parsing is slower, and whatever tool
 * processes the parse graph needs to have ways of dealing
 * with the multiple parse interpretations.
 *
 * references:
 *
 *   [GLR]  J. Rekers.  Parser Generation for Interactive
 *          Environments.  PhD thesis, University of
 *          Amsterdam, 1992.  Available by ftp from
 *          ftp://ftp.cwi.nl/pub/gipe/reports/Rek92.ps.Z .
 *          [Contains a good description of the Generalized
 *          LR (GLR) algorithm.]
 */

#ifndef GLR_H
#define GLR_H

#include "elk_glrconfig.h"
#include "elk_parsetables.h"
#include "elk_rcptr.h"
#include "elk_useract.h"
#include "sm_objpool.h"
#include "sm_objlist.h"
#include "sm_srcloc.h"
#include "sm_sobjlist.h"

#include <stdio.h>         // FILE
#include <iostream>      // std::ostream
#include "flx_elk_config.hpp"

// fwds from other files
class LexerInterface;      // lexerint.h

// forward decls for things declared below
class StackNode;           // unit of parse state
class SiblingLink;         // connections between stack nodes
class PendingShift;        // for postponing shifts.. may remove
class ELK_EXTERN GLR;                 // main class for GLR parsing


// a pointer from a stacknode to one 'below' it (in the LR
// parse stack sense); also has a link to the parse graph
// we're constructing
class SiblingLink {
public:
  // the stack node being pointed-at; it was created eariler
  // than the one doing the pointing
  RCPtr<StackNode> sib;

  // this is the semantic value associated with this link
  // (parse tree nodes are *not* associated with stack nodes --
  // that's now it was originally, but I figured out the hard
  // way that's wrong (more info in compiler.notes.txt));
  // this is an *owner* pointer
  SemanticValue sval;

  // the source location of the left edge of the subtree rooted
  // at this stack node; this is in essence part of the semantic
  // value, but automatically propagated by the parser
  SOURCELOC( SourceLoc loc; )

  // number of times this 'sval' has been yielded; this is used
  // to track cases where we yield a value and then merge it
  // (which means the induced parse forest is incomplete)
  YIELD_COUNT( int yieldCount; )

  // if you add additional fields, they need to be inited in the
  // constructor *and* in StackNode::addFirstSiblingLink_noRefCt

public:
  SiblingLink(StackNode *s, SemanticValue sv
              SOURCELOCARG( SourceLoc L ) );
  ~SiblingLink();

  #if GLR_SOURCELOC
    bool validLoc() const { return loc != SL_UNKNOWN; }
  #else
    bool validLoc() const { return false; }
  #endif
};


// the GLR parse state is primarily made up of a graph of these
// nodes, which play a role analogous to the stack nodes of a
// normal LR parser; GLR nodes form a graph instead of a linear
// stack because choice points (real or potential ambiguities)
// are represented as multiple left-siblings
class StackNode {
public:
  // the LR state the parser is in when this node is at the
  // top ("at the top" means that nothing, besides perhaps itself,
  // is pointing to it)
  //ItemSet const * const state;                 // (serf)
  StateId state;       // now it is an id

  // each leftSibling points to a stack node in one possible LR stack.
  // if there is more than one, it means two or more LR stacks have
  // been joined at this point.  this is the parse-time representation
  // of ambiguity (actually, unambiguous grammars or inputs do
  // sometimes lead to multiple siblings)
  ObjList<SiblingLink> leftSiblings;           // this is a set

  // the *first* sibling is simply embedded directly into the
  // stack node, to avoid list overhead in the common case of
  // only one sibling; when firstSib.sib==NULL, there are no
  // siblings
  SiblingLink firstSib;

  // number of sibling links pointing at 'this', plus the number
  // of worklists on which 'this' appears (some liberty is taken
  // in the mini-LR parser, but it is carefully documented there)
  int referenceCount;

  // how many stack nodes can I pop before hitting a nondeterminism?
  // if this node itself has >1 sibling link, determinDepth==0; if
  // this node has 1 sibling, but that sibling has >1 sibling, then
  // determinDepth==1, and so on; if this node has 0 siblings, then
  // determinDepth==1
  int determinDepth;

  union {
    // somewhat nonideal: I need access to the 'userActions' to
    // deallocate semantic values when refCt hits zero, and I need
    // to map states to state-symbols for the same reason.
    // update: now I'm also using this to support pool-based
    // deallocation in decRefCt()
    GLR *glr;

    // this is used by the ObjectPool which handles allocation of
    // StackNodes
    StackNode *nextInFreeList;
  };

  // ordinal position of the token that was being processed
  // when this stack node was created; this information is useful
  // for laying out the nodes when visualizing the GSS, but is
  // not used by the parsing algorithm itself
  NODE_COLUMN( int column; )

  // count and high-water for stack nodes
  static int numStackNodesAllocd;
  static int maxStackNodesAllocd;


private:    // funcs
  SiblingLink *
    addAdditionalSiblingLink(StackNode *leftSib, SemanticValue sval
                             SOURCELOCARG( SourceLoc loc ) );

public:     // funcs
  StackNode();
  ~StackNode();

  // ctor/dtor from point of view of the object pool user
  void init(StateId state, GLR *glr);
  void deinit();

  // internal workings of 'deinit', exposed for performance reasons
  inline void decrementAllocCounter();
  void deallocSemanticValues();

  // add a new link with the given tree node; return the link
  SiblingLink *addSiblingLink(StackNode *leftSib, SemanticValue sval
                              SOURCELOCARG( SourceLoc loc ) );

  // specialized version for performance-critical sections
  inline void
    addFirstSiblingLink_noRefCt(StackNode *leftSib, SemanticValue sval
                                SOURCELOCARG( SourceLoc loc ) );

  // return the symbol represented by this stack node;  it's
  // the symbol shifted or reduced-to to get to this state
  // (this used to be a data member, but there are at least
  // two ways to compute it, so there's no need to store it)
  SymbolId getSymbolC() const;

  // reference count stuff
  void incRefCt() { referenceCount++; }
  void decRefCt();

  // sibling count queries (each one answerable in constant time)
  bool hasZeroSiblings() const { return firstSib.sib==NULL; }
  bool hasOneSibling() const { return firstSib.sib!=NULL && leftSiblings.isEmpty(); }
  bool hasMultipleSiblings() const { return leftSiblings.isNotEmpty(); }

  // when you expect there's only one sibling link, get it this way
  SiblingLink const *getUniqueLinkC() const;
  SiblingLink *getUniqueLink() { return const_cast<SiblingLink*>(getUniqueLinkC()); }

  // retrieve pointer to the sibling link to a given node, or NULL if none
  SiblingLink *getLinkTo(StackNode *another);

  // recompute my determinDepth based on siblings,
  // but don't actually change the state
  int computeDeterminDepth() const;

  // debugging
  static void printAllocStats();
  void checkLocalInvariants() const;
};


// this is a priority queue of stack node paths that are candidates to
// reduce, maintained such that we can select paths in an order which
// will avoid yield-then-merge
class ReductionPathQueue {
public:       // types
  // a single path in the stack
  class Path {
  public:     // data
    // ---- right edge info ----
    // the rightmost state's id; we're reducing in this state
    StateId startStateId;

    // id of the production with which we're reducing
    int prodIndex;

    // ---- left edge info ----
    // the token column (ordinal position of a token in the token
    // stream) of the leftmost stack node; the smaller the
    // startColumn, the more tokens this reduction spans
    int startColumn;

    // stack node at the left edge; our reduction will push a new
    // stack node on top of this one
    StackNode *leftEdgeNode;

    // ---- path in between ----
    // array of sibling links, naming the path; 'sibLink[0]' is the
    // leftmost link; array length is given by the rhsLen of
    // prodIndex's production
    GrowArray<SiblingLink*> sibLinks;    // (array of serfs)

    // corresponding array of symbol ids so we know how to interpret
    // the semantic values in the links
    GrowArray<SymbolId> symbols;

    union {
      // link between nodes for construction of a linked list,
      // kept in sorted order
      Path *next;

      // link for free list in the object pool
      Path *nextInFreeList;
    };

  public:     // funcs
    Path();
    ~Path();

    void init(StateId startStateId, int prodIndex, int rhsLen);
    void deinit() {}
  };

private:      // data
  // head of the list
  Path *top;

  // allocation pool of Path objects
  ObjectPool<Path> pathPool;

  // parse tables, so we can decode prodIndex and also compare
  // production ids for sorting purposes
  ParseTables *tables;

private:      // funcs
  bool goesBefore(Path const *p1, Path const *p2) const;

public:       // funcs
  ReductionPathQueue(ParseTables *t);
  ~ReductionPathQueue();

  // get another Path object, inited with these values
  Path *newPath(StateId startStateId, int prodIndex, int rhsLen);

  // make a copy of the prototype 'src', fill in its left-edge
  // fields using 'leftEdge', and insert it into sorted order
  // in the queue
  void insertPathCopy(Path const *src, StackNode *leftEdge);

  // true if there are no more paths
  bool isEmpty() const { return top == NULL; }
  bool isNotEmpty() const { return !isEmpty(); }

  // remove the next path to reduce from the list, and return it
  Path *dequeue();

  // mark a path as not being used, so it will be recycled into the pool
  void deletePath(Path *p);
};


// each GLR object is a parser for a specific grammar, but can be
// used to parse multiple token streams
class ELK_EXTERN GLR {
public:
  // ---- grammar-wide data ----
  // user-specified actions
  UserActions *userAct;                     // (serf)

  // parse tables derived from the grammar
  ParseTables *tables;                      // (serf)

  // ---- parser state between tokens ----
  // I keep a pointer to this so I can ask for token descriptions
  // inside some of the helper functions
  LexerInterface *lexerPtr;                 // (serf)

  // Every node in this set is (the top of) a parser that might
  // ultimately succeed to parse the input, or might reach a
  // point where it cannot proceed, and therefore dies.  (See
  // comments at top of glr.cc for more details.)
  ArrayStack<StackNode*> topmostParsers;     // (refct list)

  // index: StateId -> index in 'topmostParsers' of unique parser
  // with that state, or INDEX_NO_PARSER if none has that state
  typedef unsigned char ParserIndexEntry;
  enum { INDEX_NO_PARSER = 255 };
  ParserIndexEntry *parserIndex;            // (owner)

  // this is for assigning unique ids to stack nodes
  int nextStackNodeId;
  enum { initialStackNodeId = 1 };

  // ---- parser state during each token ----
  // I used to have fields:
  //   int currentTokenType;
  //   SemanticValue currentTokenValue;
  //   SourceLoc currentTokenLoc;
  // but these have been now replaced by, respectively,
  //   lexerPtr->type
  //   lexerPtr->sval
  //   lexerPtr->loc

  // ---- scratch space re-used at token-level (or finer) granularity ----
  // to be regarded as a local variable of GLR::rwlProcessWorklist
  GrowArray<SemanticValue> toPass;

  // persistent array that I swap with 'topmostParsers' during
  // 'rwlShiftTerminals' to avoid extra copying or allocation;
  // this should be regarded as variable local to that function
  ArrayStack<StackNode*> prevTopmost;        // (refct list)

  // ---- allocation pools ----
  // this is a pointer to the same-named local variable in innerGlrParse
  ObjectPool<StackNode> *stackNodePool;

  // pool and list for the RWL implementation
  ReductionPathQueue pathQueue;

  // ---- user options ----
  // when true, failed parses are accompanied by some rudimentary
  // diagnosis; when false, failed parses are silent (default: true)
  bool noisyFailedParse;

  // ---- debugging trace ----
  // these are computed during GLR::GLR since the profiler reports
  // there is significant expense to computing the debug sm_strings
  // (that are then usually not printed)
  bool trParse;                             // tracingSys("parse")
  std::ostream &trsParse;                        // trace("parse")

  // track column for new nodes
  NODE_COLUMN( int globalNodeColumn; )

  // statistics on parser actions
  int detShift, detReduce, nondetShift, nondetReduce;

  // count of # of times yield-then-merge happens
  int yieldThenMergeCt;

private:    // funcs
  // comments in glr.cc
  SemanticValue duplicateSemanticValue(SymbolId sym, SemanticValue sval);
  void deallocateSemanticValue(SymbolId sym, SemanticValue sval);
  SemanticValue grabTopSval(StackNode *node);

  StackNode *findTopmostParser(StateId state);
  StackNode *makeStackNode(StateId state);
  void writeParseGraph(char const *input) const;
  void clearAllStackNodes();
  void addTopmostParser(StackNode *parser);
  void pullFromTopmostParsers(StackNode *parser);
  bool canMakeProgress(StackNode *parser);
  void dumpGSS(int tokenNumber) const;
  void dumpGSSEdge(FILE *dest, StackNode const *src,
                               StackNode const *target) const;
  void printConfig() const;
  void buildParserIndex();
  void printParseErrorMessage(StateId lastToDie);
  bool cleanupAfterParse(SemanticValue &treeTop);
  bool nondeterministicParseToken();
  static bool innerGlrParse(GLR &glr, LexerInterface &lexer, SemanticValue &treeTop);
  SemanticValue doReductionAction(
    int productionId, SemanticValue const *svals
    SOURCELOCARG( SourceLoc loc ) );

  void rwlProcessWorklist();
  SiblingLink *rwlShiftNonterminal(StackNode *leftSibling, int lhsIndex,
                                   SemanticValue /*owner*/ sval
                                   SOURCELOCARG( SourceLoc loc ) );
  int rwlEnqueueReductions(StackNode *parser, ActionEntry action,
                           SiblingLink *sibLink);
  void rwlCollectPathLink(
    ReductionPathQueue::Path *proto, int popsRemaining,
    StackNode *currentNode, SiblingLink *mustUseLink, SiblingLink *linkToAdd);
  void rwlRecursiveEnqueue(
    ReductionPathQueue::Path *proto,
    int popsRemaining,
    StackNode *currentNode,
    SiblingLink *mustUseLink);
  void rwlShiftTerminals();

  void configCheck(char const *option, bool core, bool table);

  sm_string stackSummary() const;
  void nodeSummary(sm_stringBuilder &sb, StackNode const *node) const;
  void innerStackSummary(sm_stringBuilder &sb,
                         SObjList<StackNode const> &printed,
                         StackNode const *node) const;

public:     // funcs
  GLR(UserActions *userAct, ParseTables *tables);
  ~GLR();

  // ------- primary interface -------
  // read the named grammar file (.bin extension, typically)
  void readBinaryGrammar(char const *grammarFname);

  // parse, using the token stream in 'lexer', and store the final
  // semantic value in 'treeTop'
  bool glrParse(LexerInterface &lexer, SemanticValue &treeTop);

};


#endif // GLR_H
