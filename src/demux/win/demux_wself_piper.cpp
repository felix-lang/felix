#include "demux_wself_piper.hpp"
#include <stdio.h>

namespace flx { namespace demux {

auto_handle::auto_handle()
{
  h = INVALID_HANDLE_VALUE;
}

auto_handle::~auto_handle()
{
  if(INVALID_HANDLE_VALUE == h) return; // done

  if(!CloseHandle(h))
    fprintf(stderr, "auto CloseHandle failed: %i\n", GetLastError());
}

wpipe_pair::wpipe_pair()
{
  // fprintf(stderr, "wpipe CTOR\n");
  // made bufsize 1 as we only ever read and write 1 byte at a time

  // looks like I can't use anonymous pipes with iocp. will have to
  // use a specially setup named pipe - one that allows repetitions...
  // this doesn't sound good.
  // This will be a problem if we have multiple event waiting threads.
  // If I don't get the flags right it will also be a problem if we
  // have multiple instances/apps running. I don't wan't this resource
  // to be globally visible. Flags?
  const char* pname = "\\\\.\\pipe\\flx_iocp_quitter";

  // don't actually need duplex, nor those buffers. 1 byte at a time suffices
  // I probably don't need both ends to be marked as nonblocking either, should
  // only need the read end nonblocking.

  // create pipe
  pipe[READ_END].h = CreateNamedPipe(pname,
   PIPE_ACCESS_DUPLEX | FILE_FLAG_OVERLAPPED,
   PIPE_TYPE_BYTE, 1, 256, 256, 0, NULL);

  if(INVALID_HANDLE_VALUE == pipe[READ_END].h)
  {
    fprintf(stderr, "couldn't create named pipe: %i\n", GetLastError());
    throw -1;
  }

  // this is the part that I don't like - this pipe's name isn't unique
  // and so theoretically another iocp quitter could join here. a race!

  // connect to it. note that overlapped isn't needed for write end as
  // we want to block.
  pipe[WRITE_END].h = CreateFile(pname, FILE_READ_DATA | FILE_WRITE_DATA,
    FILE_SHARE_READ | FILE_SHARE_WRITE, NULL, OPEN_EXISTING,
    FILE_ATTRIBUTE_NORMAL | FILE_FLAG_OVERLAPPED, NULL);

  if(INVALID_HANDLE_VALUE == pipe[WRITE_END].h)
  {
    fprintf(stderr, "failed to open named pipe: %i\n", GetLastError());
    throw -1;
  }

  // anonymous pipes can't be made nonblocking/iocpable on windows!
  // What a shame!
/*
  if(!CreatePipe(&pipe[READ_END], &pipe[WRITE_END], NULL, 1))
  {
    fprintf(stderr, "wpipe_pair CreatePipe failed: %i\n", GetLastError());
    throw -1;
  }
*/
}

void
wpipe_pair::write_byte()
{
  // I think I want a blocking write here.
  char  b = 1;
  DWORD bytes_written;
  // last arg is overlapped pointer, unused, we want to block.
  if(!WriteFile(pipe[WRITE_END].h, &b, 1, &bytes_written, NULL))
    fprintf(stderr, "wpipe_pair failed to write byte: %i\n",
      GetLastError());
}

void
wself_piper::install(demuxer* d, demux_callback* cb)
{
  fprintf(stderr, "wself_piper::install(%p, %p)\n", d, cb);
  iocp_demuxer* demux = static_cast<iocp_demuxer*>(d);

  // make read end non blocking and associate with iocp
  HANDLE read_end = pp.get_read_end();

#if 0
  // make the anonymous pipe non blocking. this function is for named pipes,
  // but I've heard talk that it works for anon pipes too. Nope, doesn't work.
  DWORD pipe_mode = PIPE_NOWAIT;
  if(!SetNamedPipeHandleState(read_end, &pipe_mode, NULL, NULL))
  {
    fprintf(stderr, "SetNamedPipeHandleState failed: %i\n", GetLastError());
    return; // not much to be done here.
  }
#endif

  if(0 != demux->associate_with_iocp(read_end, NULL))
  {
    fprintf(stderr, "failed to install self pipe in IOCP!!!\n");
    return; // error code?
  }

  // copy into the self pipe wakeup, for its later use.
  spw.d = demux;
  spw.cb = cb;
  spw.file = read_end;

  fprintf(stderr, "initial self pipe arm\n");
  spw.arm();
}

// wakes demuxer
void
wself_piper::wake()
{
  fprintf(stderr, "wself_piper::wake - write a byte\n");
  pp.write_byte();
}

wself_piper_wakeup::wself_piper_wakeup()
  // configure the control block for ReadFile on the read end pipe
  // will set pipe handle later, read = true
  : winfileio_control_block(INVALID_HANDLE_VALUE, NULL, 0, true),
    cb(0), d(0)
{
  // I'll probably need to reset the byte address
  fprintf(stderr, "SET UP THE PIPE HANDLE!\n");
}

// at this point the byte has already been read. we want to re-arm for future
// wakeups.
void
wself_piper_wakeup::iocp_op_finished(DWORD nbytes, ULONG_PTR udat,
    LPOVERLAPPED olp, int err)
{
  fprintf(stderr, "wself_piper_wakeup::iocp_op_finished\n");
  fprintf(stderr, "nbytes=%i, err=%i\n", nbytes, err);
  fprintf(stderr, "about to callback %p(%p)\n", cb, d);

  if(cb) cb->callback(d);

  arm();  // re-arm
}

void
wself_piper_wakeup::arm()
{
  // exec another nonblocking ReadFile on read end of pipe
  fprintf(stderr, "wself_piper_wakeup::arm\n");
  pb.buffer = &the_byte;
  pb.buffer_size = 1;
  pb.bytes_written = 0;
  if(start_overlapped())
    fprintf(stderr, "WARNING: wslef_pipe install completed immediately\n");
}

}}
