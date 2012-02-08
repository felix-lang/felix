#ifndef __FLX_DEMUX_IOCP_DEMUXER_H__
#define __FLX_DEMUX_IOCP_DEMUXER_H__

#include <flx_demux_config.hpp>
//#include <Windows.h>
// be specific - flx_rtl_config.h now jigs it so that windows.h  does not
// include winsock version 1 headers by default. this was making the order
// of inclusion of windows.h and winsock2.h significant with cl.exe.
#include <WinSock2.h>

#include "demux_demuxer.hpp"
#include "pthread_bound_queue.hpp"


namespace flx { namespace demux {

// not here? returns INVALID_SOCKET on failure.
// if *io_port == 0, then a port is chosen and returned in *io_port
SOCKET DEMUX_EXTERN create_listener_socket(int* io_port, int backlog);
// these two probably not used. move to wsockety.h
SOCKET DEMUX_EXTERN nice_accept(SOCKET listener);
SOCKET DEMUX_EXTERN nice_connect(const char* addr, int port);
int DEMUX_EXTERN set_tcp_nodelay(int s, int disable_nagle);

// ********************************************************
/// make sure you instantion ONE (1) of these before using winsock
// ********************************************************
class DEMUX_EXTERN winsock_initer
{
public:
  winsock_initer();
  ~winsock_initer();
};

// ********************************************************
/// iocp_wakeup base class for users of iocp_demuxer
/// becoming an overlapped call control block
// ********************************************************
class DEMUX_EXTERN iocp_wakeup {
protected:            // folks need to use these in win 32 calls
  OVERLAPPED  ol;
  // store wakeup error here?
  // I didn't want this to be felixy, useful though.
  void clear_overlapped();  // zero the OVERLAPPED structure
public:
  // 2 possibilities for piggybacking data. who could ask for more?
  // udat = per iocp association, olp = per overlapped function call.
  // why don't I need this in the posix version?
  virtual void iocp_op_finished( DWORD nbytes, ULONG_PTR udat,
    LPOVERLAPPED olp, int err) = 0;

  // start overlapped async operation. returns true if it finished
  // immediately. in this case there will be no iocp_op_finished wakeup.
  // assumes all args ready for call.
  virtual bool start_overlapped() = 0;

  // retrieves this pointer from OVERLAPPED pointer
  static iocp_wakeup* from_overlapped(LPOVERLAPPED olp);
};

// ********************************************************
// ********************************************************
class DEMUX_EXTERN iocp_demuxer : public demuxer {
  HANDLE    iocp;     // the io completion queue

  void get_evts(bool poll);
public:
  iocp_demuxer();
  virtual ~iocp_demuxer();

  // udat is the per IOCP object user cookie & the overlapped pointer
  // is the per overlapped operation cookie (sort of), so in the case
  // of acceptex, udat is set when the listener is associated with the
  // iocp and is passed to the subsequent acceptex iocp wakeups.
  // probably won't be used very often
  // the OVERLAPPED retrieved from the iocp is assumed to be part of
  // an iocp_wakeup - beware! returns 0 on success, -1 on failure.
  int associate_with_iocp(HANDLE obj, ULONG_PTR udat);

};

}} // namespace demux, flx
#endif
