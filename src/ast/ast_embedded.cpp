// embedded.cc            see license.txt for copyright and terms of use
// code for embedded.h

#include "ast_embedded.h"

EmbeddedLang::EmbeddedLang(ReportError *e)
  : err(e? e : &simpleReportError),
    text(),
    exprOnly(false),
    isDeclaration(false)
{}

EmbeddedLang::~EmbeddedLang()
{}
