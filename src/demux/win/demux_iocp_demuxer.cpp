#include "demux_iocp_demuxer.hpp"
#include "demux_quitter.hpp" // for clean threaded iocp takedown

#include <stdio.h>      // for printf debugging
#include <stddef.h>     // offsetof
#include <assert.h>
// shoving the win_queue in here for now

namespace flx { namespace demux {

// this could really do with auto objs. steal the strat stuff?

// add windows error processing macros. It's a bore otherwise.

// WaitForSingleObject on an kill event in the thread for thread cancel
// kill_event = CreateEvent(NULL, TRUE, FALSE, NULL); (what's that)
// SetEvent(kill_event) to invoke (?): SetEvent sets the event to the
// signalled state. Return value is success flag. GetLastError.

// do auto SOCKET wrapper, check closesocket return code.

// a completion port is a queue into which the os puts notifications of
// completed overlapped io requests. once the operation completes, a
// notification is sent to a worker thread that can process the result.
// a socket may be associated with a completion port at any point after
// creation.


// I don't see how to nicely stop a thread, I may have to have my own protocol
// to ask it to exit.

// PostQueuedCompletionStatus can be used by threads to wake up a worker
// thread. Could be handy replacement for timeout. "useful for notifying
// worker threads of external events"

// working through this: http://msdn.microsoft.com/msdnmag/issues/1000/Winsock/
// example of worker thread here
// http://msdn.microsoft.com/library/default.asp?url=/library/en-us/msmq/msmq_using_reading_msg_98j7.asp
// http://msdn.microsoft.com/library/default.asp?url=/library/en-us/fileio/fs/i_o_completion_ports.asp
// nono, use this onec
// http://msdn.microsoft.com/library/default.asp?url=/library/en-us/msmq/msmq_using_reading_msg_98j7.asp
// oh, wait they're the same
// FormatMessge

winsock_initer::winsock_initer()
{
  WSADATA wsaData;

  // strange, rt_faio_01 static doesn't work under vs without this.
  // hum.
  // fprintf(stderr, "WINSOCK INIT!!!\n");
  // apparently 2.2's the way to go
  int res= WSAStartup(MAKEWORD(2, 2), &wsaData);
  if(res!= 0)
  {
    //JS: WSAGetLastError CANNOT be called, since WSAStartup failed
    fprintf(stderr,"couldn't find usable winsock dll: %i\n", res);
    throw res;
  }
}

winsock_initer::~winsock_initer()
{
  if(WSACleanup() != 0)
  {
    fprintf(stderr,"WSACleanup failed %i\n", WSAGetLastError());
  }
}

// iocp_wakeup base class for users of iocp_demuxer
//static
iocp_wakeup*
iocp_wakeup::from_overlapped(LPOVERLAPPED olp)
{
  // calculate the address of this from overlapped member
  // suffer an obligatory offsestof warning from broken gccs.
  return (iocp_wakeup*)((char*)olp-offsetof(iocp_wakeup, ol));
}

void
iocp_wakeup::clear_overlapped()
{
  ZeroMemory(&ol, sizeof(ol));  // much better than memset, right?
}


iocp_demuxer::iocp_demuxer()
  : iocp(NULL)
{
  // Create the completion port
  // not sure what first 3 args do, but by specifying INVALID_HANDLE_VALUE
  // for the first I think I can ignore the rest (apart from the last, numthreads)
  // I still have to create the threads, but only NumberOfConcurrentThreads
  // will wake up from GetQueuedCompletionStatus at a time. This looks to be
  // slightly elastic...
// NT 3.51 doesn't let you pass null filehandle, you've got to have a dummy
// socket. keep that in mind. see InitializeIOCP in IOCPServer.cpp example
// taken from codeproject. GetSystemInfo to find out num CPUs
  //fprintf(stderr,"CreateIoCompletionPort with ONE WORKER THREAD\n");
  iocp = CreateIoCompletionPort(
    INVALID_HANDLE_VALUE,
    NULL,
    (ULONG_PTR)0,
    1       // 1 thread (zero means one for each CPU)
  );

  if(NULL == iocp)
  {
    DWORD err = GetLastError();
    fprintf(stderr,"failed to create completion port: %li\n", err);
    throw -1;
  }
}

iocp_demuxer::~iocp_demuxer()
{
  //fprintf(stderr, "~iocp (%p) NOW WITH ASYNC QUITTER!\n", iocp);
  try
  {
    demux_quitter q;
    q.quit(this);
  }
  catch(...)
  {
    fprintf(stderr, "~iocp_demuxer async quit threw exception!\n");
    // now what do we do?
  }


  if(NULL != iocp && !CloseHandle(iocp))
  {
    DWORD err = GetLastError();
    fprintf(stderr,"failed cleanup iocp: %li\n", err);
  }
}

int
iocp_demuxer::associate_with_iocp(HANDLE obj, ULONG_PTR udat)
{
  // fprintf(stderr, "associating with iocp=%p: %p, udat: %lx\n",
  //  iocp, obj, udat);

  // Any overlapped operations performed on the object will use the
  // completion port for notification. The 3rd param can be used to pass
  // per object context information. we'll just pass that back.
  if(CreateIoCompletionPort(obj, iocp, udat, 0) == NULL) {
    // adding the same obj twice without an intervening get completion
    // status wakup gets an error 87, ERROR_INVALID_PARAMETER
    fprintf(stderr,"CreateIoCompletionPort failed to register object: %li\n",
      GetLastError());
    return -1;
  }

  return 0;
}

void
iocp_demuxer::get_evts(bool poll) {
  // with multiple threads, this will actually wake up the last to
  // block (lifo)

  // get context, call worker_thread
  // need to be able to tell which thing completed, can have extra data
  // following some kind of struct
  // get this pointer

  // I guess to avoid swapping of thread context. By calling this on a given
  // completion port this thread is associated with it until exit or respec
  DWORD     nbytes;   // number of bytes in io transaction
  ULONG_PTR     udat;   // user data - not using this atm
  LPOVERLAPPED  olp;    // we get iocp_wakeup from this.

// If a socket handle associated with a completion port is closed,
// GetQueuedCompletionStatus returns ERROR_SUCCESS, with *lpOverlapped
// non-NULL and lpNumberOfBytes equal zero.

  int err = NO_ERROR;

  // No timeout. What does false mean? Eh. Could need a timeout to bring
  // the thread down.
  if(!GetQueuedCompletionStatus(iocp, &nbytes, &udat, &olp,
    (poll) ? 0: INFINITE))
  {
    // That's strange - I sometimes get my ConnectEx errors popping
    // out here (ERROR_SEM_TIMEOUT=121, ERROR_CONNECTION_REFUSED=1225)
    // it looks like my args (overlapped, etc) are still filled out, so
    // I can still awake the sleeper
    err = GetLastError();   // doco says this & not WSALastError.

    // let's see: yep - there's my overlapped
    // fprintf(stderr,"!iocp::wait: nbytes=%li, udat=%lx, io=%p, err=%i\n",
    //  nbytes, udat, olp, err);

    if(WAIT_TIMEOUT == err)
    {
      // we get this a lot now that we can poll the iocp, so no output
      // interestingly, nbytes = 1. what could that mean?
      return;         // no wakeup
    }
    else if(ERROR_OPERATION_ABORTED == err)
    {
      // that's real bad manners. Or I could just ignore it. Anyway,
      // any overlapped received is stale.
      fprintf(stderr, "WHOA!!! - disassociate before killing handle\n");
      return;         // no wakeup
    }
    else
    {
      fprintf(stderr,"GetQueuedCompletionStatus returned false: %i\n",
        err);
      // return here? relying on olp being NULL, to stop us dereffing
    }

    // I'm going to assume that there's a good wakeup, and fall through
    // We need to wakeup on some errors (like ERROR_CONNECTION_REFUSED)
    // FALL THROUGH
  }

// An IOCP is a very general event mechanism. It tells you not only about
// the completion of reads & writes, but also of pretty much any asynchronous
// event's completion. It doesn't quite fit in with my select style interfaces.
// I've got general overlapped things completing here. I don't want them to
// know about demuxers & so forth so I'll have to know about them.

  //fprintf(stderr,"HOLEY! Woke up!\n");
  //fprintf(stderr,"nbytes=%li, udat=%lx, olp=%p, err=%i\n",
  //  nbytes, udat, olp, err);

  // with polling it's normal not to get an overlapped pointer, because
  // we may simply have timed out
  assert( olp );

  // tell someone that some overlapped op finished
  iocp_wakeup*  wakeup = iocp_wakeup::from_overlapped(olp);

  // passing olp may be redundant, seeing as it's contained in iocp_wakeup
  wakeup->iocp_op_finished(nbytes, udat, olp, err);
}



// simple utility fn, shouldn't be here. creates listener on any interface.
// this could benifit from a SOCKET class. in failure returns INVALID_SOCKET
// CURRENTLY EATS ERROR, SO DON'T BOTHER CHECKING
SOCKET
create_listener_socket(int* io_port, int backlog)
{
  fprintf(stderr,"creating_listener_socket\n");
  SOCKET        listener;

  // could use WSASocket, but these seem to be turning out overlapped anyway
  // at least after tangling with overlapped functions.
  // socket returns INVALID_SOCKET on failure.
  listener = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);

  if (INVALID_SOCKET == listener)
  {
    fprintf(stderr,"listener create failed: %i\n", WSAGetLastError());
    return INVALID_SOCKET;
  }

  SOCKADDR_IN   addr;

  // msdn code examples don't zero the sockaddr. That makes me nervous.
  ZeroMemory(&addr, sizeof(addr));
  addr.sin_family = AF_INET;
  addr.sin_addr.s_addr = htonl(INADDR_ANY);
  addr.sin_port = htons(*io_port);

  // bind our name to the socket
  int         res;
  res = bind(listener, (LPSOCKADDR)&addr, sizeof(addr));

  if (SOCKET_ERROR == res)
  {
    fprintf(stderr,"bind() failed %i\n", WSAGetLastError());
    if(closesocket(listener) == SOCKET_ERROR)
    {
      fprintf(stderr,"closesocket failed on listener: %i\n",
        WSAGetLastError());
    }
    return INVALID_SOCKET;
  }

  // if user wanted port chosen tell them what it turned out to be
  if(0 == *io_port)
  {
    int namelen = sizeof(addr);
    if (getsockname(listener, (struct sockaddr *)&addr, &namelen)
      == SOCKET_ERROR)
    {
      fprintf(stderr, "getsockname failed (%i)\n", WSAGetLastError());

      if(closesocket(listener) == SOCKET_ERROR)
      {
        fprintf(stderr,"closesocket failed on listener: %i\n",
          WSAGetLastError());
      }
      return INVALID_SOCKET;
    }

    *io_port = ntohs(addr.sin_port);
  }

  // Set the socket to listen
  res = listen(listener, backlog);
  if (SOCKET_ERROR == res)
  {
    fprintf(stderr,"listen() failed %i\n", WSAGetLastError());

    if(closesocket(listener) == SOCKET_ERROR)
    {
      fprintf(stderr,"closesocket failed on listener: %i\n",
        WSAGetLastError());
    }

    return INVALID_SOCKET;
  }

  return listener;
}

// currently the following aren't used. Look forward to warnings
// about them.

// the posix version of this made the socket nonblocking.
// I don't seem to have to do that when using iocp. if you
// want to create a nonblocking socket (or overlapped) pass
// WSA_FLAG_OVERLAPPED to WSASocket. I've never had to
// actually do this. How do you make accept do this? (supposing
// you wanted to) WSAAccept doesn't have a flag for it (however
// it does let you do conditional accepting).
// There doesn't seem to be a sockopt
// returns INVALID_SOCKET on failure. eats the err.
SOCKET
nice_accept(SOCKET listener)
{
  struct sockaddr_in  remoteaddr;
  int         addrlen = sizeof(remoteaddr);
  SOCKET        s;

  // accept returns INVALID_SOCKET when it fails
  s = accept(listener, (struct sockaddr*)&remoteaddr, &addrlen);

  if(INVALID_SOCKET == s)
  {
    fprintf(stderr,"nice_accept failed (%i)\n", WSAGetLastError());
  }

  // the posix version makes the socket nonblocking here
  // we're not bothering

  return s;
}

// returns SOCKET_ERROR on failure, with err in WSAGetLastError()
static int
connect_sock(SOCKET s, const char* addr, int port)
{
  struct sockaddr_in  sock_addr;

  memset(&sock_addr, 0, sizeof(sock_addr));
  sock_addr.sin_family = AF_INET;
  sock_addr.sin_addr.s_addr = inet_addr(addr);
  sock_addr.sin_port = htons(port);

  return connect(s, (struct sockaddr *)&sock_addr, sizeof(sock_addr));
}

// returns INVALID_SOCKET on failure, eats last error with WSAGetLastError
// unlike the posix version, this does not make the socket nonblocking.
SOCKET
nice_connect(const char* addr, int port)
{
  SOCKET      s;

  if((s = socket(AF_INET, SOCK_STREAM, 0)) != INVALID_SOCKET
    && connect_sock(s, addr, port) != SOCKET_ERROR)
  {
    return s;   /* success! */
  }

  /* something happened (not as good as catch 22) */
  fprintf(stderr,"nice_connect failed (%i)\n", WSAGetLastError());

  if(INVALID_SOCKET != s && closesocket(s) == SOCKET_ERROR)
    fprintf(stderr,"nice close failed (%i)\n", WSAGetLastError());

  return INVALID_SOCKET;
}

// returns -1 on error with errno in WSAGetLastError. 0 otherwise.
// kind of crap.
int
set_tcp_nodelay(int s, int disable)
{
  BOOL  disable_nagle = (disable) ? true : false;

  int res = setsockopt(s, IPPROTO_TCP, TCP_NODELAY,
      (const char*)&disable_nagle, sizeof(disable_nagle));

  return (res == SOCKET_ERROR) ? -1 : 0;
}

}}
