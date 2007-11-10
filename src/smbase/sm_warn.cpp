// warn.cc            see license.txt for copyright and terms of use
// code for warn.h
// Scott McPeak, 1999  This file is public domain.

#include "sm_warn.h"
#include "sm_typ.h"
#include "sm_breaker.h"
#include <stdio.h>      // fopen, stderr, etc.
#include <time.h>       // time, ctime

// globals
WarningHandler warningHandler = defaultWarningHandler;
#ifdef NDEBUG
  WarnLevel logWarnLevel = (WarnLevel)(WARN_ALL - WARN_DEBUG);
  WarnLevel displayWarnLevel = WARN_NONE;
#else
  WarnLevel logWarnLevel = WARN_ALL;
  WarnLevel displayWarnLevel = WARN_ALL;
#endif


void warning(WarnLevel level, char const *message)
{
  warningHandler(level, message);
}


void defaultWarningHandler(WarnLevel level, char const *message)
{
  if (level & WARN_DEBUG) {
    // hit a breakpoint if the debugger is attached
    breaker();
  }

  if (level & logWarnLevel) {
    defaultWarningLogger(level, message);
  }

  if (level & logWarnLevel) {
    defaultWarningPrinter(level, message);
  }
}


void defaultWarningLogger(WarnLevel /*level*/, char const *message)
{
  static FILE *logfile = NULL;
  static bool failedToOpen = false;

  if (!logfile && !failedToOpen) {
    logfile = fopen("warning.log", "a");
    if (!logfile) {
      // don't keep trying
      failedToOpen = true;
    }
    else {
      // start with a timestamp
      time_t t;
      time(&t);
      int len = fprintf(logfile, "\nLog started at %s", ctime(&t));
        // note: astonishingly (bad), the sm_string returned by ctime() has
        //       a newline at the end!

      while (len--) {
        fprintf(logfile, "-");
      }
      fprintf(logfile, "\n");
    }
  }

  if (logfile) {
    // append the message to the logfile
    fprintf(logfile, "warning: %s\n", message);
    fflush(logfile);
  }
}


void defaultWarningPrinter(WarnLevel /*level*/, char const *message)
{
  fprintf(stderr, "warning: %s\n", message);
  fflush(stderr);
}


// no test code because it is my judgment that bugs in this
// module will be easily evident, and it is a very simple
// module, so it isn't worth it to separately test

