#ifndef __FLX_DEMUX_OVERLAPPED_H__
#define __FLX_DEMUX_OVERLAPPED_H__
#include "flx_demux_config.hpp"

// visual studio is quite sensitve about how you do these includes.
#include <WinSock2.h>              // Winsock2 (WSABufs, etc) must come before Windows.h
#include "demux_iocp_demuxer.hpp"  // this header file include Windows.h
#include <MSWSock.h>  // AcceptEx, TF_REUSE_SOCKET, etc

namespace flx { namespace demux {

// rename these to control block something or other
// get rid of default constructors - faio can worry about that.

// WARNING: in some "immediate completion" cases I have to call
// the finished function myself - in these cases I set the udat to 0, making
// it not very reliable. Either make sure the user understands immediate
// finish (and does it themselves) or keep a copy of udat.

// listener socket must be already associated with an IOCP
// in doing an AcceptEx, it might succeed immediately - do you still
// get the IOCP wakeup?
class DEMUX_EXTERN acceptex_control_block : public iocp_wakeup {
  enum { ACCEPTEX_ADDR_SIZE = sizeof(SOCKADDR_IN) + 16 };

public:
  SOCKET  listener, acceptor;
  // there are two of these!
  char  accept_buf[2*ACCEPTEX_ADDR_SIZE];

  virtual bool start_overlapped();

  acceptex_control_block()
    : listener(INVALID_SOCKET), acceptor(INVALID_SOCKET) {}
};

class DEMUX_EXTERN connectex_control_block : public iocp_wakeup
{
public:
  // move further back?
  int socket_err;         // outgoing

  // can have buffer to be sent on connection
  SOCKET    s;          // previously unbound socket
  const char* addy;       // ipv4 address
  int     p;          // port number

  // socket_err undefined
  connectex_control_block() : s(INVALID_SOCKET), addy(0), p(0) {}

  // see posix version of this, try to keep them in sync. give socket_err
  // initial definition that works with this?
  bool finished() { return ERROR_IO_PENDING != socket_err; }

  virtual bool start_overlapped();
};

// TransmitFile here (requires file handle)
class DEMUX_EXTERN transmitfile_control_block : public iocp_wakeup {
  SOCKET  s;
  HANDLE  file;
  DWORD flags;                // for possible socket reuse.
public:

  transmitfile_control_block(SOCKET dst)      // for reuse of socket
    : s(dst), file(NULL), flags(TF_DISCONNECT | TF_REUSE_SOCKET) {}

  transmitfile_control_block(SOCKET dst, HANDLE src)  // actual transmitfile
    : s(dst), file(src), flags(0) {}

  virtual bool start_overlapped();
};


// handles both WSASend & WSARecv
class DEMUX_EXTERN wsasocketio_control_block : public iocp_wakeup {
protected:
  enum { NUM_WBUFS = 1 }; // just one for now, but can do scattered send/recvs
  WSABUF    wbufs[NUM_WBUFS];
public:
  SOCKET    s;
  sel_param*  ppb;      // on input what you want, on output what you got
  int     error;
  bool    reading;  // else use WSASend

  // watch the memory interfaces here, going back and forth between threads.
  wsasocketio_control_block(SOCKET src, sel_param* pb, bool read);

  virtual bool start_overlapped();

  virtual void iocp_op_finished( DWORD nbytes, ULONG_PTR udat,
    LPOVERLAPPED olp, int err);
};

// winfileio_control_block, much like wsasocketio_control_block
class DEMUX_EXTERN winfileio_control_block : public iocp_wakeup {
  bool    reading;
public:
  sel_param pb;
  HANDLE    file; // I like to modify this from the outside

  // offset?
  winfileio_control_block(HANDLE f, void* buf, int len, bool read);

  virtual bool start_overlapped();

  // NB: no iocp_op_finished callback. defined by users of the class.
};

}}

#endif
