// ptreenode.h            see license.txt for copyright and terms of use
// parse tree node for experimental grammars (this isn't somthing
// Elkhound as a whole knows about--it doesn't make trees unless
// the user actions do)

#ifndef PTREENODE_H
#define PTREENODE_H

#include <stddef.h>     // NULL
#include <iostream>   // std::ostream

// for storing counts of parse trees; I try to make the code work for
// either 'int' or 'double' in this spot (e.g. I assign 0 to it
// instead of 0.0), even though 'int' overflows quickly for the highly
// ambiguous grammars
typedef double TreeCount;

class PTreeNode {
public:    // types
  // max # of children (when this is increased, more constructors
  // for PTreeNode should be added)
  enum { MAXCHILDREN = 10 };

  // printing options
  enum PrintFlags {
    PF_NONE    = 0,       // default, print types as-is
    PF_EXPAND  = 1,       // types are just LHS, dig down to find RHSs
    PF_ADDRS   = 2,       // print node virtual addresses to see sharing
  };

public:    // data
  // textual repr. of the production applied; possibly useful for
  // printing the tree, or during debugging
  char const *type;

  // instead of making explicit merge nodes (which runs afoul of the
  // yield-then-merge problem), just link alternatives together using
  // this link; this is NULL when there are no alternatives, or for
  // the last node in a list of alts
  PTreeNode *merged;

  // array of children; these aren't owner pointers because
  // we might have arbitrary sharing for some grammars
  int numChildren;
  PTreeNode *children[MAXCHILDREN];

  // # of parse trees of which this is the root; effectively this
  // memoizes the result to avoid an exponential blowup counting
  // the trees; when this value is 0, it means the count has not
  // yet been computed (any count must be positive)
  TreeCount count;

  // count of # of allocated nodes; useful for identifying when
  // we're making too many
  static int allocCount;

  // count # of times addAlternative is called; this will tell
  // the total number of local ambiguities that need to be resolved
  static int alternativeCount;

private:     // funcs
  // init fields which don't depend on ctor args
  void init();

  // helpers
  static void indent(std::ostream &out, int n);
  void innerPrintTree(std::ostream &out, int indentation, PrintFlags pf) const;
  int countMergedList() const;

public:      // funcs
  // now lots of constructors so we have one for each possible
  // number of children; the calls are automatically inserted
  // by a perl script ('make-trivparser.pl') or by the grammar
  // transformation GrammarAnalysis::addTreebuildingActions()
  PTreeNode(char const *t)
    : type(t), numChildren(0), count(0) { init(); }
  PTreeNode(char const *t, PTreeNode *ch0)
    : type(t), numChildren(1), count(0) { init(); children[0] = ch0; }
  PTreeNode(char const *t, PTreeNode *ch0, PTreeNode *ch1)
    : type(t), numChildren(2), count(0) { init(); children[0] = ch0; children[1] = ch1; }
  PTreeNode(char const *t, PTreeNode *ch0, PTreeNode *ch1, PTreeNode *ch2)
    : type(t), numChildren(3), count(0) { init(); children[0] = ch0; children[1] = ch1; children[2] = ch2; }
  PTreeNode(char const *t, PTreeNode *ch0, PTreeNode *ch1, PTreeNode *ch2, PTreeNode *ch3)
    : type(t), numChildren(4), count(0) { init(); children[0] = ch0; children[1] = ch1; children[2] = ch2; children[3] = ch3; }
  PTreeNode(char const *t, PTreeNode *ch0, PTreeNode *ch1, PTreeNode *ch2, PTreeNode *ch3, PTreeNode *ch4)
    : type(t), numChildren(5), count(0) { init(); children[0] = ch0; children[1] = ch1; children[2] = ch2; children[3] = ch3; children[4] = ch4; }
  // be sure to update MAXCHILDREN, above, if you add constructors
  // which accept more children

  ~PTreeNode() { allocCount--; }

  // count the number of trees encoded (taking merge nodes into
  // account) in the tree rooted at 'this'
  TreeCount countTrees();

  // print the entire parse forest using indentation to represent
  // nesting, and duplicating printing of shared subtrees within
  // ambiguous regions
  void printTree(std::ostream &out, PrintFlags pf = PF_NONE) const;

  // add an alternative to the current 'merged' list
  void addAlternative(PTreeNode *alt);
};

#endif // PTREENODE_H
