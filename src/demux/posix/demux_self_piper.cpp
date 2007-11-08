#include "demux_self_piper.hpp"
#include <stdio.h>              // printf, perror
#include <unistd.h>             // pipe for self-pipe trick.
#include <assert.h>

namespace flx { namespace demux {

auto_fd::auto_fd()
{
    fd = -1;        // invalid
}

auto_fd::~auto_fd()
{
    if(-1 == fd) return;

    if(close(fd) == -1)
        perror("auto fd close");
}

void
self_piper::install(demuxer* d, demux_callback* cb)
{
    //fprintf(stderr, "installing self piper in %p with cb=%p\n", d, cb);
    posix_demuxer* demux = static_cast<posix_demuxer*>(d);
    spw.s = pp.get_read_end();
    spw.cb = cb;

    int res = demux->add_socket_wakeup(&spw, PDEMUX_READ);
    assert(-1 != res);
}

// wake the demuxer referenced in install
void
self_piper::wake()
{
    // fprintf(stderr, "self_piper::wake\n");
    pp.write_byte();
}

void
selfpipe_wakeup::wakeup(posix_demuxer& demux)
{
    // fprintf(stderr, "selfpipe wakeup: read the pending byte and re-arm\n");
    // not using the pipe pair because it doesn't know that it's part of
    // one. not to worry.
    ssize_t         nbytes;
    char            b;

    // if this were read then this fn would work with non-sockets
    // EH? It IS read.
    nbytes = read(s, &b, 1);

    if(nbytes == -1) perror("read");

    // fprintf(stderr, "GOT: %li, %x\n", nbytes, b);
    assert(nbytes == 1 && b == 1);

    // callback!
    if(cb) cb->callback(&demux);

    // add self back! this happens even when we're quitting, but that
    // doesn't seem to matter.
    // fprintf(stderr, "selfpiper rearming\n");
    int res = demux.add_socket_wakeup(this, PDEMUX_READ);
    assert(-1 != res);
}

pipe_pair::pipe_pair()
{
  // fprintf(stderr, "creating pipe for self-pipe trick\n");

  int         self_pipe_fds[2];
  if(pipe(self_pipe_fds) == -1)
  {
      perror("ts_select_demuxer::self_pipe");
      throw -1;
  }

  // fprintf(stderr, "self pipe fds: read: %i, write: %i\n",
  //  self_pipe_fds[0], self_pipe_fds[1]);

  fds[0].fd = self_pipe_fds[0];
  fds[1].fd = self_pipe_fds[1];
}

void
pipe_pair::write_byte()
{
    char    b = 1;
    ssize_t nbytes;
    // is this blocking? I guess it has to be...
    nbytes = write(fds[1].fd, &b, 1);       // wake up, jeff!

    // fprintf(stderr, "self_piper::wake write returned: %i\n", nbytes);

    if(-1 == nbytes) perror("pipe_pair::write_byte");
    assert(1 == nbytes);
}

int
pipe_pair::get_read_end()
{
  return fds[0].fd;
}

} }
