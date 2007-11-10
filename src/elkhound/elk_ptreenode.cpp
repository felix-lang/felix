// ptreenode.cc            see license.txt for copyright and terms of use
// code for ptreenode.h

#include "elk_ptreenode.h"
#include "sm_typ.h"
#include "sm_str.h"
#include "sm_trace.h"

#include <string.h>         // strchr

int PTreeNode::allocCount = 0;
int PTreeNode::alternativeCount = 0;


void PTreeNode::init()
{
  merged = NULL;
  allocCount++;
}


TreeCount PTreeNode::countTrees()
{
  // memoize to avoid exponential blowup
  if (count != 0) {
    return count;
  }

  else {
    // a single tree can have any possibility for each of
    // its children, so the result is their product
    count = 1;
    for (int i=0; i<numChildren; i++) {
      count *= children[i]->countTrees();
    }

    // are there alternatives?
    if (merged) {
      // add them too (recurse down the list of alts)
      count += merged->countTrees();
    }
  }

  return count;
}


void PTreeNode::printTree(std::ostream &out, PrintFlags pf) const
{
  if (tracingSys("ptreeAddrs")) {
    pf = (PrintFlags)(pf | PF_ADDRS);
  }
  innerPrintTree(out, 0 /*indentation*/, pf);
}


// amount to indent per level
enum { INDENT_INC = 2 };

void PTreeNode::innerPrintTree(std::ostream &out, int indentation,
                               PrintFlags pf) const
{
  int alts = 1;
  sm_string LHS;

  if (merged) {
    // this is an ambiguity node
    alts = countMergedList();

    // since all of the alternatives should rewrite the same LHS
    // nonterminal, extract it from the first one
    char const *firstSpace = strchr(type, ' ');
    if (!firstSpace) {
      LHS = type;     // no space, use whole thing
    }
    else {
      LHS = sm_string(type, firstSpace-type);
    }

    indentation += INDENT_INC;
  }

  // iterate over interpretations
  int ct=1;
  for (PTreeNode const *n = this; n != NULL; n = n->merged) {
    if (alts > 1) {
      indent(out, indentation - INDENT_INC);
      out << "--------- ambiguous " << LHS << ": "
          << ct << " of " << alts << " ---------\n";
    }

    indent(out, indentation);

    out << n->type;
    if (pf & PF_EXPAND) {
      // the type is just the LHS name; write out the RHS names
      // after an "->"
      if (n->numChildren) {
        out << " ->";
        for (int c=0; c < n->numChildren; c++) {
          out << " " << n->children[c]->type;
        }
      }
    }

    if (pf & PF_ADDRS) {
      // print the parse tree node address, so I can verify proper sharing
      out << " (" << ((void*)n) << ")";
    }
    out << "\n";

    // iterate over children
    for (int c=0; c < n->numChildren; c++) {
      // recursively print children
      n->children[c]->innerPrintTree(out, indentation + INDENT_INC, pf);
    }

    ct++;
  }

  if (merged) {
    // close up ambiguity display
    indentation -= INDENT_INC;
    indent(out, indentation);
    out << "--------- end of ambiguous " << LHS << " ---------\n";
  }
}

STATICDEF void PTreeNode::indent(std::ostream &out, int n)
{
  for (int i=0; i<n; i++) {
    out << " ";
  }
}

// # of nodes on the 'merged' list; always at least 1 since
// 'this' is considered to be in that list
int PTreeNode::countMergedList() const
{
  int ct = 1;
  for (PTreeNode const *n = merged; n != NULL; n = n->merged) {
    ct++;
  }
  return ct;
}


void PTreeNode::addAlternative(PTreeNode *alt)
{
  // insert as 2nd element
  alt->merged = this->merged;
  this->merged = alt;

  alternativeCount++;
}
