// reporterr.cc            see license.txt for copyright and terms of use
// code for reporterr.h

#include "ast_reporterr.h"

#include <iostream>       // std::cout


// --------------------- SimpleReportError -------------------------
void SimpleReportError::reportError(char const *str)
{
  std::cout << "error: " << str << std::endl;
}

void SimpleReportError::reportWarning(char const *str)
{
  std::cout << "warning: " << str << std::endl;
}

SimpleReportError simpleReportError;


// --------------------- SilentReportError -------------------------
void SilentReportError::reportError(char const *str)
{}

void SilentReportError::reportWarning(char const *str)
{}

SilentReportError silentReportError;


// EOF
