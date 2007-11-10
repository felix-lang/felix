// warn.h            see license.txt for copyright and terms of use
// module to facilitate providing operational warnings to the user
// Scott McPeak, 1999  This file is public domain.

#ifndef __WARN_H
#define __WARN_H

// note: In retrospect, this module was either a bad idea, or I didn't
//       implement it well.  Either way, don't use it for anything new.

// non-disjoint warning classification scheme
// (add more classes as necessary)
enum WarnLevel {
  WARN_PERFORMANCE     = 0x01,
    // may cause suboptimal performance

  WARN_SECURITY        = 0x02,
    // possible compromise of private data, unauthrorized
    // access, authentication warning, etc.

  WARN_COMPATIBILITY   = 0x04,
    // interoperability with other software (including
    // different versions of this software) may be
    // adversely affected

  WARN_DEBUG           = 0x08,
    // of use during debugging only; setting this flag means
    // the warning handler should alert an attached debugger

  WARN_INFORMATION     = 0x10,
    // I'm not sure when/why this would be used...
    // Note: This is *not* to be used as a diagnostic 'printf'.

  WARN_ALL             = 0x1F,
    // logical-or of all flags

  WARN_NONE            = 0x0,
    // no warnings
};


// user interface
// --------------
// call this to report a warning
//   level   - logical-or of applicable conditions
//   message - user-intelligible message (should *not* contain a newline)
void warning(WarnLevel level, char const *message);


// handler interface
// -----------------
// the warning() function calls warningHandler(), so new
// handlers are installed by changing that value
typedef void (*WarningHandler)(WarnLevel level, char const *message);
extern WarningHandler warningHandler;


// default handler
// ---------------
// the default warning handler masks the input level with two
// global variables:
//   logWarnLevel - messages are written to a log file, "warning.log"
//   displayWarnLevel - messages are written to stderr via stdio 'stderr'
extern WarnLevel logWarnLevel;       // default: WARN_ALL, minus WARN_DEBUG ifdef NDEBUG
extern WarnLevel displayWarnLevel;   // default: ifdef NDEBUG, WARN_NONE, else WARN_ALL

// handler functions (handler dispatches to logger and printer)
void defaultWarningHandler(WarnLevel level, char const *message);
void defaultWarningLogger(WarnLevel level, char const *message);
void defaultWarningPrinter(WarnLevel level, char const *message);


#endif // __WARN_H

