// asockind.h            see license.txt for copyright and terms of use
// AssocKind; pulled out on its own so I don't have dependency problems

#ifndef ASOCKIND_H
#define ASOCKIND_H

#include "sm_str.h"

// specifies what to do when there is a shift/reduce conflict, and
// the production and token have the same precedence; this is attached
// to the token
enum AssocKind {
  AK_LEFT,            // disambiguate by reducing
  AK_RIGHT,           // disambiguate by shifting
  AK_NONASSOC,        // make it a parse-time syntax error
  AK_NEVERASSOC,      // make it a parsgen-time specification error
  AK_SPLIT,           // (GLR-specific) fork the parser

  NUM_ASSOC_KINDS
};

sm_string toString(AssocKind k);

#endif // ASOCKIND_H

