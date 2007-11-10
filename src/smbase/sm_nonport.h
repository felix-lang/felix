// nonport.h            see license.txt for copyright and terms of use
// collection of nonportable routines (the interfaces
//   are portable, but the implementations are not)
// Scott McPeak, Dan Bonachea 1998-1999  This file is public domain.

#ifndef __NONPORT_H
#define __NONPORT_H

#include "sm_typ.h"
#include <stdarg.h>  // va_list


// I'm attempting to improve error handling in this module; this fn will be
// called when a syscall fails, *in addition* to whatever error behavior
// is documented here (e.g., a fn might call this, and then return false).
// The default behavior is to do nothing.  In sftpc and sftpd, I plan to
// point this at xSysError::xsyserror (see syserr.h).
typedef void (*NonportFailFunc)(char const *syscallName, char const *context);
  // syscallName  - name of failing system call
  // context      - current activity (maybe just calling fn's name) or NULL
extern NonportFailFunc nonportFail;

// this is default handler
void defaultNonportFail(char const *syscallName, char const *context);



// put terminal into 'raw' or 'cooked' mode
void setRawMode(bool raw);

// get the next character typed without buffering or echoing; needs the
// console to be in 'raw' mode
char getConsoleChar();


// get a millisecond count, where 0 is an unspecified event
long getMilliseconds();


// remove all priviledges to a file, except for read/write
// access by the file's owner; returns false on error
bool limitFileAccess(char const *fname);


// get process id; meaning is somewhat system-dependent, but the goal
// is to return something that can be used to correlate log output
// from (say) sftpd with log output from some other source (syslog,
// or NT event viewer, etc.)
#if 0
int getProcessId();
#endif

// create a new directory; returns false on error;
// precise naming semantics, such as use
// of 'current working directory', etc., are specified by the
// underlying OS's mkdir (or equivalent) command (it is hoped
// this underspecification will not be a problem in practice)
bool createDirectory(char const *dirname);

// change to a directory; returns false on failure
// again, current-directory semantics are unspecified
bool changeDirectory(char const *dirname);

// retrieve the name of the current working directory
// (more best effort crap, I guess)
bool getCurrentDirectory(char *dirname, int dirnameLen);


// get and process the names of files *and directories* in the current directory
typedef bool (*PerFileFunc)(char const *name, void *extra);
  // name   - file/dir being processed (contains no slashes)
  // extra  - 2nd parameter to applyToCwdContents
  // return - true to continue, false to stop iterating
void applyToCwdContents(PerFileFunc func, void *extra=NULL);

// same as above, but in an explicitly named directory
void applyToDirContents(char const *dirName,
                        PerFileFunc func, void *extra=NULL);


// return true if the given sm_string names a directory
bool isDirectory(char const *path);


// delete a file; returns false on failure
bool removeFile(char const *fname);


// retrieve the current date
void getCurrentDate(int &month, int &day, int &year);
  // month:    1 = January ... 12 = December
  // day:      1 = first day of month, ...
  // year:     1999 is when this being coded
  // e.g., February 8, 1999  is  month=2, day=8, year=1999


// sleep for a bit (low resolution)
void portableSleep(unsigned seconds);


/*
// determine usable name of current user, and write it into 'buffer'
void getCurrentUsername(char *buffer, int buflen);
*/

// read a sm_string from the console, with no echo
void readNonechoString(char *buffer, int buflen, char const *prompt);


// return true if a file or directory exists
bool fileOrDirectoryExists(char const *name);


// ensure that the pathname part of a file name exists;
// it creates missing directories as necessary, with only
// user rwx permission; if 'isDirectory' is true, the whole
// name is also verified as a directory; returns false on
// error
bool ensurePath(char const *filename, bool isDirectory);


// returns true if the system has a cryptographically-
// secure random number generator
bool hasSystemCryptoRandom();

// if the above fn returns true, this will retrieve a
// random 32-bit integer; may block until the bits
// become available
unsigned getSystemCryptoRandom();


// determine how many characters, *not* including the final NUL, would
// be written by vsprintf; this is allowed to overestimate
int vnprintf(char const *format, va_list args);

// this is implemented in terms of vnprintf, so not technically
// a function with "nonportable implementation", but it belongs
// here anyway
int nprintf(char const *format, ...);


#endif // __NONPORT_H

