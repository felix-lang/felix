// genml.h            see license.txt for copyright and terms of use
// extension to gramanl module that generates ML instead of C

#ifndef GENML_H
#define GENML_H

class GrammarAnalysis;

// entry point
void emitMLActionCode(GrammarAnalysis const &g, char const *mliFname,
                      char const *mlFname, char const *srcFname);

#endif // GENML_H
