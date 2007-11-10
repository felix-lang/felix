// asthelp.cc            see license.txt for copyright and terms of use
// code for what's declared in asthelp.h

#include "ast_asthelp.h"
#include "sm_strutil.h"

// ----------- debugPrint helpers -----------------------
std::ostream &ind(std::ostream &os, int indent)
{
  while (indent--) {
    os << " ";
  }
  return os;
}


void debugPrintStr(sm_string const &s, char const *name,
                   std::ostream &os, int indent)
{
  ind(os, indent) << name << " = " << quoted(s) << "\n";
}


template <class STR>
void debugPrintStringList(ASTList<STR> const &list, char const *name,
                          std::ostream &os, int indent)
{
  ind(os, indent) << name << ": ";
  {
    int ct=0;
    FOREACH_ASTLIST(STR, list, iter) {
      if (ct++ > 0) {
        os << ", ";
      }
      os << quoted(*( iter.data() ));
    }
  }
  os << "\n";
}


void debugPrintList(ASTList<sm_string> const &list, char const *name,
                    std::ostream &os, int indent)
{
  debugPrintStringList(list, name, os, indent);
}

void debugPrintList(ASTList<LocString> const &list, char const *name,
                    std::ostream &os, int indent)
{
  debugPrintStringList(list, name, os, indent);
}


// ----------- xmlPrint helpers -----------------------
void xmlPrintStr(sm_string const &s, char const *name,
                 std::ostream &os, int indent)
{
  ind(os, indent) << "<member type=sm_string name = \"" << name << "\">\n";
  // dsw: quoted might add another layer of quotes.
  ind(os, indent+2) << "<value type=sm_string val=\"" << quoted(s) << "\" />\n";
  ind(os, indent) << "</member>\n";
}


template <class STR>
void xmlPrintStringList(ASTList<STR> const &list, char const *name,
                        std::ostream &os, int indent)
{
  ind(os, indent) << "<member type=sm_stringList name = \"" << name << "\">\n";
  {
    FOREACH_ASTLIST(STR, list, iter) {
      // dsw: quoted might add another layer of quotes.
      ind(os, indent+2) << "<object type=sm_string val=\"" << quoted(*( iter.data() )) << "\" />\n";
    }
  }
  ind(os, indent) << "</member>\n";
}


void xmlPrintList(ASTList<sm_string> const &list, char const *name,
                  std::ostream &os, int indent)
{
  xmlPrintStringList(list, name, os, indent);
}

void xmlPrintList(ASTList<LocString> const &list, char const *name,
                  std::ostream &os, int indent)
{
  xmlPrintStringList(list, name, os, indent);
}
