#include "demux_overlapped.hpp"
#include <stdio.h>      // fprintf
#include <assert.h>

// cygwin's copy of mswsock.h leaves something to be desired...
#ifndef WSAID_CONNECTEX
typedef
BOOL
(PASCAL FAR * LPFN_CONNECTEX) (
    IN SOCKET s,
    IN const struct sockaddr FAR *name,
    IN int namelen,
    IN PVOID lpSendBuffer OPTIONAL,
    IN DWORD dwSendDataLength,
    OUT LPDWORD lpdwBytesSent,
    IN LPOVERLAPPED lpOverlapped
    );

#define WSAID_CONNECTEX \
    {0x25a207b9,0xddf3,0x4660,{0x8e,0xe9,0x76,0xe5,0x8c,0x74,0x06,0x3e}}
#endif

namespace flx { namespace demux {

// windows includes files here? vs will be fussy.

// AcceptEx

// return async finished flag (error flags - can be transmitted via class)
// AcceptEx is the way to get accept connections via the IOCP
bool
acceptex_control_block::start_overlapped()
{
  clear_overlapped();

// I've seen two examples get the pointer to AcceptEx, just in case it
// isn't implemented...
  // fprintf(stderr,"AcceptExing: listen backlog => can succeed immediately\n");

  // this is only set when acceptex receives data and returns immediately.
  // can't hurt to set it.
  DWORD nbytes = 0;
  BOOL  success;

  // note that in order to get the wakeup packet, the listener must
  // already be associated with the iocp. for future async io, the acceptor
  // must be associated too.
  success = AcceptEx(listener, acceptor,
    accept_buf,       // required - near/far address
    0,            // receive data size - don't yet want this
    ACCEPTEX_ADDR_SIZE,   // must be nonzero
    ACCEPTEX_ADDR_SIZE,   // must be nonzero
    &nbytes,        // only set if fn completes. should be 0
    &ol);         // oblig. gets us back to the this ptr

  // if there is a backlog of connections, AcceptEx can return immediately
  if(success)
  {
    // must clear the wait
    fprintf(stderr,"WHOA! AcceptEx RETURNED SUCCESS IMMEDIATELY!\n");
    fprintf(stderr, "This could be bad, as wait should call op_finish\n");
    fprintf(stderr, "We also lose the udat cookie (set to NULL)\n");
    // handle the successful wakeup
    // complete_async_op(demux, drv, nbytes, NO_ERROR);
    // I hope they don't want the udat pointer, because I
    // just made it up (0=NULL). Not using it anyway.
    iocp_op_finished(nbytes, 0, &ol, NO_ERROR);
    return false;   // means no completion packet coming (correct?)
  }
  else
  {
    int err = WSAGetLastError();
    // can also return WSACONNRESET, which isn't so bad
    if(ERROR_IO_PENDING == err)
    {
      // fprintf(stderr,"AcceptEx returned ERROR_IO_PENDING - that's normal\n");
      // This is the normal situation, fall through, leaving thread
      // to sleep on wakeup call.
    }
    else
    {
      fprintf(stderr,"AcceptEx failed: %i\n", err);
      fprintf(stderr,"returning true should wake thread to detect failure.\n");
      return true;    // have self woken
    }
  }
  return false;       // async not finished

}

// ConnectEx
#if 0
// apparently we're supposed to do this now to make the acceptee inherit
// the listener's state. it is currently in the default state
//err = setsockopt( sAcceptSocket,
//  SOL_SOCKET,
//  SO_UPDATE_ACCEPT_CONTEXT,
//  (char *)&sListenSocket,
//  sizeof(sListenSocket) );
#endif

// what a pain in the arse (zzz)
// This doesn't exist in win2000, so it'll need to be synchronous there.
static int
GetConnectExAddr(SOCKET s, LPFN_CONNECTEX* conn_fn)
{
  *conn_fn = NULL;
  GUID      GuidConnectEx = WSAID_CONNECTEX;
  DWORD     dwBytes;
  int       err;

  err = WSAIoctl(s,   // why do I need this?
    SIO_GET_EXTENSION_FUNCTION_POINTER,
    &GuidConnectEx,
    sizeof(GuidConnectEx),
    conn_fn,
    sizeof(*conn_fn),
    &dwBytes,
    NULL, NULL);    // no overlapped, no completion fun ptr
//  fprintf(stderr,"Get addr dwbytes: %li\n", dwBytes);
  return err;
}

// this is the weirdest. To use ConnectEx, the socket must be already bound.
// By trial and error, I found that it had to be bound to INADDR_ANY: 0.
// So strange. Apparently I don't have to do it again if I want to reuse.
static int
bind_socket(SOCKET s)
{
  SOCKADDR_IN   addr;

  ZeroMemory(&addr, sizeof(addr));
  addr.sin_family = AF_INET;
  addr.sin_addr.s_addr = htonl(INADDR_ANY);
  addr.sin_port = htons(0);

  return bind(s, (LPSOCKADDR)&addr, sizeof(addr));
}

bool
connectex_control_block::start_overlapped()
{
  clear_overlapped();

  // why not get this directly from the ConnectEx?
  socket_err = ERROR_IO_PENDING;


  DWORD bytes_sent = 0;   // we're not sending data on connect (yet)
  BOOL  success;

  LPFN_CONNECTEX  pfConnectEx;

  // unfortunate, will fix up later.
  // fprintf(stderr,"Getting ConnectEx address\n");

  // Turns out that ConnectEx isn't defined anywhere; I have to load its
  // addr via WSAIoctl
  // this is a bad way. make the driver cache it. why on earth is this
  // call per-socket? does it really need to be that way?
  if(GetConnectExAddr(s, &pfConnectEx) == SOCKET_ERROR)
  {
    fprintf(stderr,"GetConnectExAddr failed: %i\n", WSAGetLastError());
    return true;
  }

  // fprintf(stderr,"about to connectex to %s:%i, %i\n", addy, p, s);

  // this is so strange - I have to bind the socket to the localhost.
  // if I don't, ConnectEx returns EINVAL. in any case, I won't need
  // to do this again if I reuse this socket.
  if(bind_socket(s) == SOCKET_ERROR)
    fprintf(stderr,"ConnectEx bind failed: %i\n", WSAGetLastError());

  // I hope ConnectEx doesn't want this to hang around, because it's
  // going to drop off the stack after this.
  SOCKADDR_IN   addr;

  // some examples don't zero the addr. That makes me nervous.
  ZeroMemory(&addr, sizeof(addr));
  addr.sin_family = AF_INET;
  addr.sin_addr.s_addr = inet_addr(addy);
  addr.sin_port = htons(p);

  // in order to receive the wakeup packet, s must already be associated
  // with the iocp. this is best done at socket creation time. for these
  // sockets it's probably best to also bind them at the same time.
  // that requires "purposed" sockets (CreateConnectSocket?).
  // p.s. the default (waio_base) wakeup is doing fine for now.

  success = (*pfConnectEx)(s, // socket
    (LPSOCKADDR)&addr,    // connect address
    sizeof(addr),     // size thereof
    NULL,         // not sending any data yet, but we could
    0,            // ditto
    NULL,         // should be zero until this changes
    &ol);         // oblig. gets us back to the this ptr

// there's a caveat about the type of socket s becomes after ConnectEx.
// It's in some kind of default state and cannot be used with shutdown
// change it with setsockopt (?)
  if(success)
  {
    fprintf(stderr,"WHOA! ConnectEx RETURNED SUCCESS IMMEDIATELY!\n");
    fprintf(stderr, "BAD: calls op_finish and loses udat cookie\n");
    // handle the successful wakeup. (udat=0, olp=&ol)
    iocp_op_finished(bytes_sent, 0, &ol, NO_ERROR);
    return false;   // means no completion packet coming (correct?)
  }
  else
  {
    int err = WSAGetLastError();

    if(ERROR_IO_PENDING == err)
    {
      // fprintf(stderr,"ConnectEx pending...\n");
      // This is the normal situation, fall through, leaving thread
      // to sleep on wakeup call.
    }
    else
    {
      // maybe store the error here. that could work for all
      // windows wakeups
      fprintf(stderr,"ConnectEx failed: %i\n", err);
      return true;    // have self woken
    }
  }
  return false;       // not finished
}

// TransmitFile

bool
transmitfile_control_block::start_overlapped()
{
  clear_overlapped();

  // 0 bytes => transmit entire file
  // the second zero means use the default chunk size
  // the NULL is for mem buffers to bookend the file with. nothing yet.
  // the final zero is for various flags, including a way of doing
  // DisconnectEx style socket reuse (more widely compatible?)

  // in order to receive the wakeup, s must already be associated with the
  // iocp. this is best done at socket creation time.
  if(TransmitFile(s, file, 0, 0, &ol, NULL, flags))
  {
    fprintf(stderr,"Transmit file succeeded immediately! waking...\n");
    return true;
  }
  else
  {
    DWORD err = WSAGetLastError();

    // will need to actually signal something
    // fprintf(stderr,"signal TransmitFile failure!\n");
    if(ERROR_IO_PENDING != err && WSA_IO_PENDING != err)
      fprintf(stderr,"genuine error from TransmitFile: %li\n", err);
  }
  return false;
}


// SOCKET io using WSASend and WSARecv

// windows style control blocks
wsasocketio_control_block::wsasocketio_control_block(SOCKET src, sel_param* pb,
  bool inread)
  : s(src), ppb(pb), reading(inread)
{
}

bool
wsasocketio_control_block::start_overlapped()
{
  clear_overlapped();

  error = 0;

  // num bytes received IF recv completes immediately.
  DWORD imm_bytes;
  int   recv_res;

  // set up the single wbuf, bearing in mind we may be part way.
  wbufs[0].len = ppb->buffer_size - ppb->bytes_written;
  wbufs[0].buf = ppb->buffer + ppb->bytes_written;

// fprintf(stderr, "sockio: %p->finished = %i, reading = %i\n",
//  ppb, ppb->finished(), reading);

  // Ideally, we would like to be able to use MSG_WAITALL, which would
  // let us only get a completion packet when either all the data was
  // available or the connection had been closed or shutdown.
  // Unfortunately this is not possible for non-blocking sockets, so
  // we have to take whatever we get and then call WSARecv again.

  //#define MSG_WAITALL 0   // not defined in cygwin - apparently this
  //DWORD flags = MSG_WAITALL;

  // ah, unfortunately MSG_WAITALL is not supported for non blocking sockets
  // we'll just have to do it ourselves
  DWORD flags = MSG_PARTIAL;

  // completion routines! (unused)
  if(reading)
    recv_res = WSARecv(s, wbufs, NUM_WBUFS, &imm_bytes, &flags, &ol, NULL);
  else
    recv_res = WSASend(s, wbufs, NUM_WBUFS, &imm_bytes, flags, &ol, NULL);

  // don't know if I need to check non winsock errs

  switch(recv_res)
  {
    case 0:
    {
      // flags are updated to indicate what? if there was a callback, it
      // would be scheduled to be called when this thread is in the
      // waitable state, whatever that means.
      //fprintf(stderr,
      //  "WSA%s completed immediately!!! nbytes: %li, flags: %lx\n",
      //    (reading) ? "Recv" : "Send", imm_bytes, flags);

      // looks like we get the completion packet even if we do finish
      // immediately so let the iocp wake us. note that this method
      // of manually calling iocp_op_finished is not so great as we
      // don't know the (as yet unused) udat cookie and so set it to 0.
      // fprintf(stderr, "calling finished manually (ppb=%p)\n", ppb);
      // iocp_op_finished(imm_bytes, 0, &ol, NO_ERROR);

      // false because iocp_op_finished will wake us. I guess false from
      // these guys means that a completion packet is in the mail and
      // true means that it isn't (in the mail).
      return false;
    }
    break;
    case SOCKET_ERROR:
    {
      DWORD err = WSAGetLastError();

      // normal mode - wait for completion
      // fyi, xp pro seems to mostly give us ERROR_IO_PENDING
      if(ERROR_IO_PENDING == err || WSA_IO_PENDING == err)
      {
        // fprintf(stderr,"WSA%s pending completion (%li)\n",
        //  (reading) ? "Recv" : "Send", err);
        return false;
      }

      fprintf(stderr,"WSARecv/Send returned SOCKET_ERR: %li\n", err);
      ppb->eof_detected=true;
      return true;    // assume it's bad and we won't get a wakeup
    }
    break;
    default:
    {
      fprintf(stderr,"WSARecv/Send returned other error: %i, GetLastError: %li\n",
        recv_res, GetLastError());
      return true;        // wake up
    }
    break;
  }

  return false;
}

// NB: called by iocp::wait, so be aware of which thread is doing what, lest
// you be surprised by this being called asynchronously.
void
wsasocketio_control_block::iocp_op_finished(DWORD nbytes, ULONG_PTR udat,
  LPOVERLAPPED olp, int err)
{
  error = err;        // copy back for others to look at.
                // perhaps move back to iocpwakeup
// fprintf(stderr, "wsasocketio::finished: ppb=%p, nbytes=%li, err=%i, read=%i\n",
//  ppb, nbytes, err, reading);

  if(err)
  {
    fprintf(stderr, "wsasocketio, got error: %i\n", err);
  }

  // fprintf(stderr,"wsa_socketio wakeup, nb: %li, err: %i\n", nbytes, err );
assert( !ppb->finished() );
  // keep track of bytes received.
  ppb->bytes_written += nbytes;

  if(0 == nbytes)
  {
    fprintf(stderr, "wsaiosocket got zero bytes: err=%i, read=%i\n",
      err, reading);
  }

  // if we're not finished, we have to reinstall our request
  // zero bytes indicates shutdown/closure, right?
  // might be using this for WSASend. Instead of broken pipes on win32,
  // instead we get WSAECONNRESET (pretty sure) on write. On read?
// not sure about this - I don't think we have to check nbytes == 0
  if(0 == nbytes || ppb->finished())
  {
    return;
  }
  else
  {
    // go back around again
    fprintf(stderr,"didn\'t get everything (%li of %li bytes)\n",
      ppb->bytes_written, ppb->buffer_size);
    if(start_overlapped())
    {
      fprintf(stderr, "UM, socket restart finished immediately\n");
      fprintf(stderr, "causes new wakeup? or should I loop around?\n");
    }
  }
}


// file/pipe io using ReadFile and WriteFile

winfileio_control_block::winfileio_control_block(HANDLE f, void* buf, int len,
  bool inread)
  : reading(inread), file(f)
{
  // pb is not so useful here. we only want to
  // know num bytes written/processed.
  pb.buffer = (char*)buf;
  pb.buffer_size = len;
  pb.bytes_written = 0;
}

// if file is opened with FILE_FLAG_OVERLAPPED, we can do "immutable file ptr"
// ops & set the desired offset within the overlapped. can also stick an
// event to signal in there.
bool
winfileio_control_block::start_overlapped()
{
  // fprintf(stderr,"winfileio_cb::start_overlapped: reading=%i\n", reading);
  clear_overlapped();

  // DWORD  imm_bytes;
  BOOL  success;

  // don't need bytes read, written when we have an OVERLAPPED
  if(reading)
    // success = ReadFile(file, pb.buffer, pb.buffer_size, &imm_bytes, &ol);
    success = ReadFile(file, pb.buffer, pb.buffer_size, NULL, &ol);
  else
    //success = WriteFile(file, pb.buffer, pb.buffer_size, &imm_bytes, &ol);
    success = WriteFile(file, pb.buffer, pb.buffer_size, NULL, &ol);

  // fprintf(stderr,"immbytes = %li\n", imm_bytes);

  if(!success)
  {
    int err = GetLastError();

    // I'm getting this for unfinished
    if(ERROR_IO_PENDING == err)
    {
       return false;  // sleep on
    }
    else
    {
       fprintf(stderr,"%sFile failed! (%li)\n", (reading) ? "Read" : "Write",
         err);
       return true;      // ask for wakeup
    }

    // fprintf(stderr,"%sFile failed! (%li)\n", (reading) ? "Read" : "Write",
    //  GetLastError());
    // fprintf(stderr,"do I still get completion packet???\n");
    // assume not
  }

  return false;       // sleep on
}

}}
