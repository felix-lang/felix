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

// looks a bit like wsa_socketio (bad name, sends too)
class FAIO_EXTERN winfile_io
  : public waio_base, public demux::winfileio_control_block
{
public:
  winfile_io()      // flx linkage
    : winfileio_control_block(NULL, NULL, 0, false){}

  // offset?
  winfile_io(demux::iocp_demuxer *iod_a,HANDLE f, void* buf, int len, bool read)
    : waio_base(iod_a), winfileio_control_block(f, buf, len, read) {}

  bool start_async_op_impl();

  virtual void iocp_op_finished( DWORD nbytes, ULONG_PTR udat,
    LPOVERLAPPED olp, int err);
};


}}
#endif  // __DWINIO__
