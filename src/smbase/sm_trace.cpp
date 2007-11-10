// trace.cc            see license.txt for copyright and terms of use
// code for trace.h

#include "sm_trace.h"
#include "sm_objlist.h"
#include "sm_str.h"
#include "sm_strtokp.h"
#include "sm_nonport.h"

#include <fstream>   // ofstream
#include <stdlib.h>    // getenv


// auto-init
static bool inited = false;

// list of active tracers, initially empty
static ObjList<sm_string> tracers;

// stream connected to /dev/null
std::ofstream devNullObj("/dev/null");
static std::ostream *devNull = &devNullObj;


// initialize
static void init()
{
  if (inited) {
    return;
  }

  // there's a more efficient way to do this, but I don't care to dig
  // around and find out how
  // this leaks, and now that I'm checking for them, it's a little
  // annoying...
  //devNull = new ofstream("/dev/null");

  inited = true;
}


void traceAddSys(char const *sysName)
{
  init();

  tracers.prepend(new sm_string(sysName));
}


void traceRemoveSys(char const *sysName)
{
  init();

  MUTATE_EACH_OBJLIST(sm_string, tracers, mut) {
    if (mut.data()->compareTo(sysName) == 0) {
      mut.deleteIt();
      return;
    }
  }
  xfailure("traceRemoveSys: tried to remove system that isn't there");
}


bool tracingSys(char const *sysName)
{
  init();

  FOREACH_OBJLIST(sm_string, tracers, iter) {
    if (iter.data()->compareTo(sysName) == 0) {
      return true;
    }
  }
  return false;
}


void traceRemoveAll()
{
  tracers.deleteAll();
}


std::ostream &trace(char const *sysName)
{
  init();

  if (tracingSys(sysName)) {
    std::cout << "%%% " << sysName << ": ";
    return std::cout;
  }
  else {
    return *devNull;
  }
}


void trstr(char const *sysName, char const *traceString)
{
  trace(sysName) << traceString << std::endl;
}


std::ostream &traceProgress(int level)
{
  if ( (level == 1) ||
       (level == 2 && tracingSys("progress2")) ) {
    static long progStart = getMilliseconds();

    return trace("progress") << (getMilliseconds() - progStart) << "ms: ";
  }
  else {
    return *devNull;
  }
}


void traceAddMultiSys(char const *systemNames)
{
  StrtokParse tok(systemNames, ",");
  loopi(tok) {
    if (tok[i][0] == '-') {
      // treat a leading '-' as a signal to *remove*
      // a tracing flag, e.g. from some defaults specified
      // statically
      char const *name = tok[i]+1;
      if (tracingSys(name)) {      // be forgiving here
        traceRemoveSys(name);
      }
      else {
        std::cout << "Currently, `" << name << "' is not being traced.\n";
      }
    }

    else {
      // normal behavior: add things to the trace list
      traceAddSys(tok[i]);
    }
  }
}


bool traceProcessArg(int &argc, char **&argv)
{
  traceAddFromEnvVar();

  if (argc >= 3  &&  0==std::strcmp(argv[1], "-tr")) {
    traceAddMultiSys(argv[2]);
    argc -= 2;
    argv += 2;
    return true;
  }
  else {
    return false;
  }
}


bool ignoreTraceEnvVar = false;

void traceAddFromEnvVar()
{
  if (ignoreTraceEnvVar) {
    return;
  }

  char const *var = getenv("TRACE");
  if (var) {
    traceAddMultiSys(var);
  }

  ignoreTraceEnvVar = true;
}


// EOF
