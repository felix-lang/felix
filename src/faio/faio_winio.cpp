#include "faio_winio.hpp"
#include <stdio.h>      // printf

using namespace flx::demux;
namespace flx { namespace faio {

// way of adding sockets to the IOCP.
bool
iocp_associator::start_async_op_impl()
{
  fprintf(stderr,"iocp_associator: start async_op_impl\n");

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
  fprintf(stderr,"wasync_accept: start async_op_impl\n");
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
  fprintf(stderr,"connect_ex: start async_op_impl\n");
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
  fprintf(stderr,"wasync_transmit_file: start async_op_impl\n");
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
  fprintf(stderr,"wsa_socketio: start async_op_impl\n");
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
// Doing the handling myself - this can restart the the op giving us
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

// file io

bool
winfile_io::start_async_op_impl()
{
  fprintf(stderr,"winfile_io: start async_op_impl\n");
  return start_overlapped();  // go
}

// this too could be factored back, I think. that waio_base finished
// would have to change. it's just asking for for some fn to realise
// that it's finished.
// the byte count shouldn't need to be updated here...
void
winfile_io::iocp_op_finished( DWORD nbytes, ULONG_PTR udat,
  LPOVERLAPPED olp, int err)
{
  // fprintf(stderr,"winfile_io wakeup, nb: %li, err: %i\n", nbytes, err );

  // fprintf(stderr,"THIS WAKEUP SHOULD BE THE SAME AS SEND/RECV make it so\n");
  // actually, I don't think it should be. the SOCKET stuff goes around
  // the loop again (and ignores errors, check it doesn't hammer).

  // keep track of bytes received.
  pb.bytes_written += nbytes;

  waio_base::iocp_op_finished(nbytes, udat, olp, err);
}

}}
