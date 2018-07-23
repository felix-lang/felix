Debugging
=========

Type Checking
+++++++++++++

.. code-block:: felix

  stmt := "type-error" stmt
  stmt := "type-assert" stmt

These two statements are a programming aid which allows the programmer
to write a dummy statement which is expected to contain a type error
or not, respectively.

The type-error form causes a compile time abort of the statement does
not contain a type error.  Its primary use is pedagogical, to show
type errors in syntax coloured code.

The type-assert form causes a compile time error if the statement
does not type check. If the statement does type check, it is
removed from the program, generating no actual code.
Its primary use is to validate presence of required overloads.
Note that in Felix type checking and overload resolution are
the same process.




Tracing
-------

.. code-block:: felix

  stmt := "trace" sname sstring

The trace statement emits a debug message. Trace code generation is disabled by default.
Consider this program:

.. code-block:: felix

  proc checker() {
    trace checkname "Trace checker";
    println$ "Checker run";
  }

  for i in 1..3 perform checker;

Here is a run:

.. code-block:: bash 

  ~/felix>flx --force  x
  Checker run
  Checker run
  Checker run

To enable trace code generation the `FLX_ENABLE_TRACE` switch must be set for
C++ compilation:

.. code-block:: bash

  ~/felix>flx --force --cflags=-DFLX_ENABLE_TRACE x
  1 : TRACE: Trace checker
  Felix location: /Users/skaller/felix/x.flx 3[3]-3[34]
  C++ location  : /Users/skaller/.felix/cache/text/Users/skaller/felix/x.cpp 72
  Checker run
  2 : TRACE: Trace checker
  Felix location: /Users/skaller/felix/x.flx 3[3]-3[34]
  C++ location  : /Users/skaller/.felix/cache/text/Users/skaller/felix/x.cpp 72
  Checker run
  3 : TRACE: Trace checker
  Felix location: /Users/skaller/felix/x.flx 3[3]-3[34]
  C++ location  : /Users/skaller/.felix/cache/text/Users/skaller/felix/x.cpp 72
  Checker run

UDP Tracing
-----------

Tracing via UDP is available on Unix systems, it is not currently implemented
for Windows.

.. code-block:: felix

  Debug::enable_local_udp_trace;

  proc checker() {
    Debug::send_udp_trace_message "HIHI";
    println$ "Checker run";
  }

  for i in 1..3 perform checker;


UDP tracing must be enabled. This creates a UPD socket on port 1153 
of IP address `127.0.0.1`.

The output from the UDP socket can be monitored by the C++ program 
`flx_udp_trace_monitor`. This is a stand-alone C++ program found
in the `src/tools` directory. You have to compile this by hand.

.. code-block:: bash

  ~/felix>clang++ src/tools/flx_udp_trace_monitor.cxx
  ~/felix>./a.out
  UDP Trace Monitor Listening on port 1153

Make sure to start the monitor in a terminal first.
Now you can run the Felix program:

.. code-block:: bash

  ~/felix>flx --force  x
  Bound Trace Output Socket OK!
  First UDP Trace message sent OK! 4 bytes = 'HIHI'
  Checker run
  Checker run
  Checker run

The monitor will show:

.. code-block:: text

  Received = 4
  Buffer = HIHI
  Received = 4
  Buffer = HIHI
  Received = 4
  Buffer = HIHI


