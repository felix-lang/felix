Package: src/packages/faio.fdoc


=============================
Faio: Felix Async I/O support
=============================

=================== =================================
key                 file                              
=================== =================================
faio_drv.hpp        share/src/faio/faio_drv.hpp       
faio_posixio.hpp    share/lib/rtl/faio_posixio.hpp    
faio_posixio.cpp    share/src/faio/faio_posixio.cpp   
faio_winio.hpp      share/lib/rtl/faio_winio.hpp      
faio_winio.cpp      share/src/faio/faio_winio.cpp     
faio_timer.hpp      share/lib/rtl/faio_timer.hpp      
faio_timer.cpp      share/src/faio/faio_timer.cpp     
faio.py             $PWD/buildsystem/faio.py          
timer.fpc           $PWD/src/config/timer.fpc         
unix_faio.fpc       $PWD/src/config/unix/faio.fpc     
win_faio.fpc        $PWD/src/config/win/faio.fpc      
flx_faio_config.hpp share/lib/rtl/flx_faio_config.hpp 
=================== =================================


Faio Driver
===========


.. code-block:: cpp

  //[faio_drv.hpp]
  #ifndef __FLX_FAIO_DRV_H__
  #define __FLX_FAIO_DRV_H__
  #include <flx_faio_config.hpp>
  
  #include "pthread_bound_queue.hpp"
  #include "demux_timer_queue.hpp"
  #include "demux_demuxer.hpp"
  
  namespace flx { namespace faio {
  
  // this may be needed but I've lost track of where
  // we get SIGPIPE, SIG_IGN from ..
  
  #if 0
  void FAIO_EXTERN sigpipe_ignorer()
  {
      void (*prev_handler)(int);  // solaris is FUN.
      prev_handler = signal(SIGPIPE, SIG_IGN);
  
      if(SIG_ERR == prev_handler)
      {
          fprintf(stderr, "failed to install SIGPIPE ignorer\n");
          throw -1;
      }
      else if(prev_handler != SIG_IGN && prev_handler != SIG_DFL)
      {
          fprintf(stderr,"warning: blew away prev SIGPIPE handler: %p\n",
              prev_handler);
      }
  }
  #endif
  
  }}
  #endif


Faio I/O
========


.. index:: FAIO_EXTERN(class)
.. index:: FAIO_EXTERN(class)
.. index:: FAIO_EXTERN(class)
.. index:: FAIO_EXTERN(class)
.. code-block:: cpp

  //[faio_posixio.hpp]
  #ifndef __FLX_FAIO_POSIXIO_H__
  #define __FLX_FAIO_POSIXIO_H__
  #include <flx_faio_config.hpp>
  
  #include "flx_async.hpp"
  
  // we don't need to piggyback much data at all. for now just the demuxer,
  // so that we can be woken up, and the buffer info (this replaces the
  // felix "socket" thread type, which was ugly.
  
  #include "demux_posix_demuxer.hpp"
  #include "demux_timer_queue.hpp"
  
  namespace flx { namespace faio {
  
  class FAIO_EXTERN socketio_wakeup : public demux::socket_wakeup {
  public:
    demux::sel_param   pb;     // in: what you want, out: what you get
    int       sio_flags;  // either one of PDEMUX_{READ|WRITE}A
    class socketio_request *request;
  
    virtual void wakeup(demux::posix_demuxer& demux);
  };
  
  // this can handle most unix style io, that is, read & write on sockets,
  // files & pipes. NICE. the fact that the socket is now in here may mean
  // I can get rid of the epoll hack
  // Not sure if this can be used for file fds.
  class FAIO_EXTERN socketio_request : public ::flx::async::flx_driver_request_base {
  public:
      socketio_wakeup sv;
      demux::posix_demuxer *pd;
      socketio_request() {}       // Lord Felix demands it. Like STL.
      socketio_request(socketio_request const&);
      void operator = (socketio_request const&);
  
      socketio_request(demux::posix_demuxer *pd_a, int s, char* buf, long len, bool r);
      bool start_async_op_impl();
  };
  
  // client open
  class FAIO_EXTERN connect_request
    : public ::flx::async::flx_driver_request_base, public demux::connect_control_block {
  public:
    demux::posix_demuxer *pd;
    connect_request() {}      // flx linkage
  
    connect_request(demux::posix_demuxer *pd_a,const char* addr, int port);
    bool start_async_op_impl();
    virtual void wakeup(demux::posix_demuxer&);
  };
  
  // server open
  class FAIO_EXTERN accept_request
    : public ::flx::async::flx_driver_request_base, public demux::accept_control_block {
  public:
    // we sometimes know that there'll be several connections to accept.
    // this'll need a different wakeup - and a different interface between
    // event source & wakeups
  
    demux::posix_demuxer *pd;
    accept_request() {} // flx linkage
  
    // eeh, give that a better name
    accept_request(demux::posix_demuxer *pd_a, int listener) : pd(pd_a) { s = listener; }
  
    // from flx_driver_request_base
    bool start_async_op_impl();
  
    // from accept_control_block
    virtual void wakeup(demux::posix_demuxer& demux);
  };
  
  }}
  #endif

.. code-block:: cpp

  //[faio_posixio.cpp]
  #include <stdio.h>      // printf
  #include "faio_posixio.hpp"
  #include "demux_sockety.hpp"    // async_connect
  
  #include <sys/types.h>  // getsockopt & co
  #include <sys/socket.h>
  
  #include <unistd.h>     // close
  #include <string.h>     // strerror - probably not portable
  #include <assert.h>
  
  using namespace flx::demux;
  namespace flx { namespace faio {
  
  connect_request::connect_request(demux::posix_demuxer *pd_a,const char* addr, int port) :pd(pd_a) { addy = addr; p = port; s=-1; }
  
  socketio_request::socketio_request(demux::posix_demuxer *pd_a, int s, char* buf, long len, bool read)
  : pd(pd_a)
  {
    //fprintf(stderr,"socketio_request %p making socketio_wakeup for socket %d\n",this,s);
    sv.s = s;
    sv.request = this;
    // demux supports reading AND writing. We don't. Yet.
    sv.sio_flags = ((read) ? PDEMUX_READ : PDEMUX_WRITE);
  
    sv.pb.buffer = buf;
    sv.pb.buffer_size = len;
    sv.pb.bytes_written = 0;        // really bytes_processed
  }
  
  socketio_request::socketio_request(socketio_request const &a) : pd(a.pd)
  {
    //fprintf(stderr, "copying socketio_request to %p\n",this);
    sv = a.sv;
    sv.request = this;
  }
  
  // EXTREME HACKERY!
  void socketio_request::operator=(socketio_request const &a)
  {
    //fprintf(stderr, "assigning socketio_request to %p\n",this);
  
    flx_driver_request_base::operator=(a);
    sv = a.sv;
    sv.request = this;
    pd = a.pd;
  }
  
  bool
  socketio_request::start_async_op_impl()
  {
    //fprintf(stderr,"socketio_request: socket %d start async_op_impl %p\n",sv.s,this);
    // fprintf(stderr, "adding wakeup: len %i, done %i\n",
    //   sv.pb.buffer_size, sv.pb.bytes_written);
  
    if(sv.s == -1) {
      fprintf(stderr, "Attempt to start_async_op on socket -1\n");
      exit(1);
    }
  
    // wake thread if call failed
    bool failed = (pd->add_socket_wakeup(&sv, sv.sio_flags) == -1);
    if (failed)
      fprintf(stderr,"socketio_request FAILED %p, sock=%d, dir=%d\n",this, sv.s, sv.sio_flags);
    //else
    //  fprintf(stderr,"socketio_request OK %p\n",this);
    return failed;
  }
  
  
  void
  socketio_wakeup::wakeup(posix_demuxer& demux)
  {
    //fprintf(stderr, "Wakeup, socket = %d\n",s); 
    // handle read/write, return true if not finished.
    // otherwise wakeup return false.
    bool  connection_closed;
  
    //fprintf(stderr, "making socketio_wakeup %p\n",this);
    //fprintf(stderr,"prehandle wakeup, this: %p, read: %i, len: %i, done %i\n",
    //  this, read, pb.buffer_size, pb.bytes_written);
  
    // NOTE: this code does not handle the possibility of both read AND
    // write being set. That would require thinking about the what
    // the connect_closed return value meant. In any case, we don't
    // do that stuff here yet.
  
    if(wakeup_flags & PDEMUX_ERROR)
    {
      connection_closed = true;
      //pb.bytes_written=0;
      fprintf(stderr,"posix faio wakeup PDEMUX_ERROR, connection closed = %d\n", connection_closed);
    }
  
    else if(wakeup_flags & PDEMUX_EOF)
    {
      connection_closed = true;
      fprintf(stderr,"posix faio wakeup PDEMUX_EOF, connection closed = %d\n", connection_closed);
      //pb.bytes_written=0;
    }
  
    else if(wakeup_flags & PDEMUX_READ)
    {
      // just check that our above assumption hasn't been violated.
      assert(wakeup_flags == PDEMUX_READ);
      //fprintf(stderr,"posix faio wakeup PDEMUX_READ, reading..\n");
      connection_closed = posix_demuxer::socket_recv(s, &pb);
      //fprintf(stderr,"posix faio wakeup PDEMUX_READ, connection closed = %d\n", connection_closed);
    }
    else
    {
      // never hurts to be paranoid.
      assert(wakeup_flags == PDEMUX_WRITE);
      //fprintf(stderr,"posix faio wakeup PDEMUX_WRITE, writing..\n");
      connection_closed = posix_demuxer::socket_send(s, &pb);
      //if(connection_closed)
      //  fprintf(stderr,"posix faio wakeup PDEMUX_WRITE, connection closed = %d\n", connection_closed);
    }
  
    // fprintf(stderr,"posthandle wakeup, this: %p, read: %i, len: %i, done %i\n",
    //  this, read, pb.buffer_size, pb.bytes_written);
    // fprintf(stderr,"wakeup of %p, closed = %i\n", this, connection_closed);
  
    // wake up: time to process some data
    if(connection_closed || pb.bytes_written == pb.buffer_size)
    {
      // fprintf(stderr,"schedding %p, drv: %p, f: %p\n", this, drv, f);
      // if the connection closed, this notify should tell the caller
      // not to keep trying to write, but it doesn't .. why not?
      // who called it anyhow?
      // I think the writing code ignores error returns ..
      request->notify_finished();
      return;
    }
  
    // fprintf(stderr,"not schedding %p\n", this);
    fprintf(stderr, "Incomplete request on %d, waiting for more I/O\n",s);
    if(demux.add_socket_wakeup(this, sio_flags) == -1)
    fprintf(stderr,"failed to re-add_socket_wakeup\n");
  }
  
  // asynchronous connect
  bool
  connect_request::start_async_op_impl()
  {
    //fprintf(stderr,"connect_request %p: start async_op_impl\n",this);
  
    // call failed or finished (!), wake up thread as no wakeup coming
    if(start(*pd) == -1) {
      fprintf(stderr, "FAILED TO SPAWN CONNECT REQUEST\n");
      return true;
    }
  
    // NONONONONO! Referring to this's variables after a successful start
    // gives rise to a race condition, which is bad.
    //fprintf(stderr, "CONNECT REQUEST SPAWNED\n");
    return false;     // do not reschedule after a successful start
  
  /*
    // I've not seen this yet, don't know why.
    if(0 == socket_err) fprintf(stderr, "WOW, instant CONNECT\n");
  
    // call didn't fail, could be pending or finished.
    // return socket_err != EINPROGRESS, the contrapositive, sort of
    return 0 == socket_err;   // no err => finished immediately
  */
  }
  
  void
  connect_request::wakeup(posix_demuxer& demux)
  {
    //fprintf(stderr, "connect_request::wakeup\n");
  
    // fprintf(stderr,"connect woke up\n");
    connect_control_block::wakeup(demux);
  
    // felix thread can pick out error itself.
    notify_finished();
  }
  
  
  // async accept
  bool
  accept_request::start_async_op_impl()
  {
    //fprintf(stderr,"accept_request %p: start async_op_impl\n",this);
    bool failed = (start(*pd) == -1);      // accept_control_block function
    if(failed)
      fprintf(stderr, "FAILED TO SPAWN ACCEPT REQUEST\n");
    //else
    //  fprintf(stderr, "ACCEPT REQUEST SPAWNED\n");
    return failed;
  }
  
  void
  accept_request::wakeup(posix_demuxer& demux)
  {
    // does the leg work.
    accept_control_block::wakeup(demux);
    //'fprintf(stderr, "faio_posix::accept_request::wakeup\n");
  
    if(accepted == -1)
    {
      // I don't know if this is a good idea...
      fprintf(stderr, "accept request failed (%i), retrying...\n",
        socket_err);
      // didn't get it - go back to sleep
      if(start(demux) == -1)
        fprintf(stderr, "failed again... probably was a bad idea\n");
      return;
    }
  
    notify_finished();
  }
  
  }}

.. index:: FAIO_EXTERN(class)
.. index:: FAIO_EXTERN(class)
.. index:: FAIO_EXTERN(class)
.. index:: FAIO_EXTERN(class)
.. index:: FAIO_EXTERN(class)
.. index:: FAIO_EXTERN(class)
.. code-block:: cpp

  //[faio_winio.hpp]
  #ifndef __FLX_FAIO_WINIO_H__
  #define __FLX_FAIO_WINIO_H__
  #include <flx_faio_config.hpp>
  
  // visual studio is quite sensitve about how you do these includes.
  // THIS is the way (WinSock2.h must include Windows.h).
  #include <WinSock2.h>
  #include <MSWSock.h>        // AcceptEx, TF_REUSE_SOCKET, etc
  
  #include "flx_async.hpp"
  #include "demux_overlapped.hpp"   // nicely wrapped async windows calls
  
  namespace flx { namespace faio {
  
  // interestingly, because in windows the async objects are associated
  // with an IOCP before their use, we don't actually need a demuxer here
  // at all. That's kind of nice. (actually iocp_associator uses it now)
  
  // a flx driver request to the add socket s to the drivers iocp
  // this is currently the only windows driver request that uses the demuxer.
  class FAIO_EXTERN iocp_associator : public ::flx::async::flx_driver_request_base {
    SOCKET  s;
  public:
    demux::iocp_demuxer *iod;
    // should have result & errcode
    iocp_associator() : iod(0) {} // shouldn't this also set s?
    iocp_associator(demux::iocp_demuxer *iod_a, SOCKET associatee)
    : s(associatee), iod(iod_a) {}
  
    bool start_async_op_impl();
  };
  
  // flx <-> c++ stuff for async io (well, it was)
  
  // transition to new windows async control block
  class FAIO_EXTERN waio_base : public ::flx::async::flx_driver_request_base {
  protected:
    ::flx::async::finote_t *fn_a;
  public:
    demux::iocp_demuxer *iod;
    bool  success;          // eh?
  
    waio_base() : iod(0), success(false) {}
    waio_base(demux::iocp_demuxer *iod_a) : iod(iod_a), success(false) {}
  
    // actually wakes up thread
    virtual void iocp_op_finished( DWORD nbytes, ULONG_PTR udat,
      LPOVERLAPPED olp, int err);
  };
  
  
  // listener socket must be already associated with an IOCP
  // in doing an AcceptEx, it might succeed immediately - do you still
  // get the IOCP wakeup?
  class FAIO_EXTERN wasync_accept
    : public waio_base, public demux::acceptex_control_block
  {
  public:
    wasync_accept() {}  // felix linkage demands it
  
    wasync_accept(demux::iocp_demuxer *iod_a,SOCKET l, SOCKET a) : waio_base(iod_a) { listener = l; acceptor = a; }
  
    bool start_async_op_impl();
  
    virtual void iocp_op_finished( DWORD nbytes, ULONG_PTR udat,
      LPOVERLAPPED olp, int err);
  };
  
  class FAIO_EXTERN connect_ex
    : public waio_base, public demux::connectex_control_block
  {
  public:
  
    connect_ex() {}     // flx linkage
  
    connect_ex(demux::iocp_demuxer *iod_a,SOCKET soc, const char* addr, int port)
      : waio_base(iod_a) { s = soc; addy = addr; p = port; }
  
    bool start_async_op_impl();
  
    virtual void iocp_op_finished( DWORD nbytes, ULONG_PTR udat,
      LPOVERLAPPED olp, int err);
  };
  
  // TransmitFile here (requires file handle)
  class FAIO_EXTERN wasync_transmit_file
    : public waio_base, public demux::transmitfile_control_block
  {
  public:
    wasync_transmit_file()
      : waio_base(0), transmitfile_control_block(INVALID_SOCKET, NULL) {}   // flx linkage
  
    wasync_transmit_file(demux::iocp_demuxer *iod_a,SOCKET dst)      // for reuse of socket
      : waio_base(iod_a), transmitfile_control_block(dst) {}
  
    wasync_transmit_file(demux::iocp_demuxer *iod_a,SOCKET dst, HANDLE src)  // actual transmitfile
      : waio_base(iod_a), transmitfile_control_block(dst, src) {}
  
    // from flx_request_base
    bool start_async_op_impl();
  
    virtual void iocp_op_finished(DWORD nbytes, ULONG_PTR udat,
      LPOVERLAPPED olp, int err);
  };
  
  // handles both WSASend & WSARecv
  class FAIO_EXTERN wsa_socketio
    : public waio_base, public demux::wsasocketio_control_block
  {
  public:
    wsa_socketio()
      : wsasocketio_control_block(INVALID_SOCKET, NULL, false) {}
  
    wsa_socketio(demux::iocp_demuxer *iod_a,SOCKET src, demux::sel_param* ppb, bool read)
      : waio_base(iod_a), wsasocketio_control_block(src, ppb, read) {}
  
    bool start_async_op_impl();
  
    virtual void iocp_op_finished( DWORD nbytes, ULONG_PTR udat,
      LPOVERLAPPED olp, int err);
  };
  
  
  }}
  #endif  // __DWINIO__

.. code-block:: cpp

  //[faio_winio.cpp]
  #include "faio_winio.hpp"
  #include <stdio.h>      // printf
  
  using namespace flx::demux;
  namespace flx { namespace faio {
  
  // way of adding sockets to the IOCP.
  bool
  iocp_associator::start_async_op_impl()
  {
    //fprintf(stderr,"iocp_associator: start async_op_impl\n");
  
    // nasty: note how I'm making the user cookie constant (0).
    if(iod->associate_with_iocp((HANDLE)s, 0) != 0)
      fprintf(stderr,"associate request failed - get result here!\n");
  
    return true;      // wake caller
  }
  
  void
  waio_base::iocp_op_finished( DWORD nbytes, ULONG_PTR udat,
    LPOVERLAPPED olp, int err)
  {
    // fprintf(stderr,"general wakeup thing - rescheduling\n");
    //fprintf(stderr,"this: %p, q: %p, f: %p, err: %i\n", this, q, f, err);
  
    // this tells us when things went wrong (store it)
    if(NO_ERROR != err)
      fprintf(stderr,"catchall wakeup got error: %i (should store it)\n", err);
  
    success = (NO_ERROR == err);  // this works pretty well
    notify_finished();
  }
  
  bool
  wasync_accept::start_async_op_impl()
  {
    //fprintf(stderr,"wasync_accept: start async_op_impl\n");
    return start_overlapped();
  }
  
  void
  wasync_accept::iocp_op_finished( DWORD nbytes, ULONG_PTR udat,
    LPOVERLAPPED olp, int err)
  {
    waio_base::iocp_op_finished(nbytes, udat, olp, err);
  }
  
  
  bool
  connect_ex::start_async_op_impl()
  {
    //fprintf(stderr,"connect_ex: start async_op_impl\n");
    return start_overlapped();
  }
  
  void
  connect_ex::iocp_op_finished( DWORD nbytes, ULONG_PTR udat,
    LPOVERLAPPED olp, int err)
  {
    waio_base::iocp_op_finished(nbytes, udat, olp, err);
  }
  
  
  bool
  wasync_transmit_file::start_async_op_impl()
  {
    //fprintf(stderr,"wasync_transmit_file: start async_op_impl\n");
    return start_overlapped();
  }
  
  void
  wasync_transmit_file::iocp_op_finished( DWORD nbytes, ULONG_PTR udat,
    LPOVERLAPPED olp, int err)
  {
    waio_base::iocp_op_finished(nbytes, udat, olp, err);
  }
  
  bool
  wsa_socketio::start_async_op_impl()
  {
    //fprintf(stderr,"wsa_socketio: start async_op_impl\n");
    return start_overlapped();    // start overlapped op
  }
  
  // this could be factored into demux... or it might need
  // to stay here... this is really a finished that isn't finished
  // same goes for winfileio (I think)
  void
  wsa_socketio::iocp_op_finished( DWORD nbytes, ULONG_PTR udat,
    LPOVERLAPPED olp, int err)
  {
    // fprintf(stderr,"wsa_socketio wakeup, nb: %li, err: %i\n", nbytes, err );
  // Doing the handling myself - this can restart the op giving us
  // a possible race condition... or not? It should be sync with this call.
    // wsasocketio_control_block::iocp_op_finished(nbytes, udat, olp, err);
  
    ppb->bytes_written += nbytes;
  
    // if we're not finished, we have to reinstall our request
    // zero bytes indicates shutdown/closure, right?
    // might be using this for WSASend. Instead of broken pipes on win32,
    // instead we get WSAECONNRESET (pretty sure) on write. On read?
    if(0 == nbytes || ppb->finished())
    {
      // this'll wake us up
      waio_base::iocp_op_finished(nbytes, udat, olp, err);
    }
    else
    {
      // go back around again
      // this returns a finished flag (bad idea). it can also fail.
      // I think it would be better to know that.
      if(start_overlapped())
        fprintf(stderr, "socketio restart finished! WHAT TO DO!?!\n");
    }
  }
  
  }}


Faio Timer
==========


.. index:: FAIO_EXTERN(class)
.. code-block:: cpp

  //[faio_timer.hpp]
  #ifndef __FLX_FAIO_TIMER_H__
  #define __FLX_FAIO_TIMER_H__
  #include <flx_faio_config.hpp>
  
  #include "demux_demuxer.hpp"        // sel_param, demuxer base
  #include "flx_async.hpp"
  #include "demux_timer_queue.hpp"
  
  #include "flx_rtl.hpp"
  
  namespace flx { namespace faio {
  
  
  // sleeping
  class FAIO_EXTERN sleep_request
    : public ::flx::async::flx_driver_request_base, public demux::sleep_task
  {
    demux::timer_queue *sleepers;
    double      delta;
  public:
    sleep_request() {}        // flx linkage
  
    sleep_request(demux::timer_queue *sleepers_a, double d) :
      sleepers(sleepers_a), delta(d)
    {}
  
    // from driver request
    bool start_async_op_impl();
  
    void fire();
  
  };
  
  }} // namespace faio, flx
  #endif

.. code-block:: cpp

  //[faio_timer.cpp]
  #include "faio_timer.hpp"
  
  using namespace flx::demux;
  namespace flx { namespace faio {
  bool
  sleep_request::start_async_op_impl()
  {
    //fprintf(stderr,"Sleep: start async_op_impl %p\n",this);
    sleepers->add_sleep_request(this, delta);
    return false;   // no wakeup
  }
  
  void sleep_request::fire() {
    //fprintf (stderr,"FIRE req=%p\n",this);
    notify_finished();
  }
  
  }}


.. code-block:: fpc

  //[timer.fpc]
  Name: Timer
  Description: Real time clock services
  Requires: faio
  includes:  '"faio_timer.hpp"'


.. code-block:: fpc

  //[unix_faio.fpc]
  Name: faio
  Description: Asynchronous I/O support
  provides_dlib: -lfaio_dynamic
  provides_slib: -lfaio_static
  includes: '"faio_posixio.hpp"'
  Requires: flx_async flx_pthread demux flx flx_gc
  library: faio
  macros: BUILD_FAIO
  srcdir: src/faio
  src: faio_(timer|posixio)\.cpp
  headers: faio_(drv|timer|posixio)\.hpp


.. code-block:: fpc

  //[win_faio.fpc]
  Name: faio
  Description: Asynchronous I/O support
  provides_dlib: /DEFAULTLIB:faio_dynamic
  provides_slib: /DEFAULTLIB:faio_static
  includes: '"faio_winio.hpp"'
  Requires: flx_async flx_pthread demux flx flx_gc
  library: faio
  macros: BUILD_FAIO
  srcdir: src/faio
  src: faio_(timer|winio)\.cpp
  headers: faio_(drv|timer|winio)\.hpp


.. code-block:: python

  #[faio.py]
  import fbuild
  from fbuild.functools import call
  from fbuild.path import Path
  from fbuild.record import Record
  
  import buildsystem
  
  # ------------------------------------------------------------------------------
  
  def build_runtime(phase):
      print('[fbuild] [faio]')
      path = Path(phase.ctx.buildroot/'share'/'src/faio')
  
      dst = 'host/lib/rtl/faio'
      srcs = [
          path / 'faio_timer.cpp',
      ]
      includes = [
          phase.ctx.buildroot / 'host/lib/rtl',
          phase.ctx.buildroot / 'share/lib/rtl'
      ]
      macros = ['BUILD_FAIO']
      libs=[
          call('buildsystem.flx_pthread.build_runtime', phase),
          call('buildsystem.flx_async.build_runtime', phase),
          call('buildsystem.demux.build_runtime', phase),
      ]
  
      if 'win32' in phase.platform:
          srcs.append(path / 'faio_winio.cpp')
          includes.append(Path('src', 'demux', 'win'))
  
      if 'posix' in phase.platform:
          srcs.append(path / 'faio_posixio.cpp')
          includes.append(Path('src', 'demux', 'posix'))
  
      return Record(
          static=buildsystem.build_cxx_static_lib(phase, dst, srcs,
              includes=includes,
              macros=macros,
              libs=[lib.static for lib in libs]),
          shared=buildsystem.build_cxx_shared_lib(phase, dst, srcs,
              includes=includes,
              macros=macros,
              libs=[lib.shared for lib in libs]))
  
  def build_flx(phase):
      return
      #return buildsystem.copy_flxs_to_lib(phase.ctx,
      #    Path('src/faio/*.flx').glob())


.. code-block:: cpp

  //[flx_faio_config.hpp]
  #ifndef __FLX_FAIO_CONFIG_H__
  #define __FLX_FAIO_CONFIG_H__
  #include "flx_rtl_config.hpp"
  #ifdef BUILD_FAIO
  #define FAIO_EXTERN FLX_EXPORT
  #else
  #define FAIO_EXTERN FLX_IMPORT
  #endif
  #endif


