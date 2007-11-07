#line 368 "../lpsrc/flx_posix_demux.ipk"
#include <stdio.h>    // printf
#include <errno.h>    // errno
#include "demux_pfileio.hpp"

// blocking reads & writes that use a worker fifo. users overload
// finished flag to implement wakeup

// if we could group the requests, we could do a scattered read
// or we could do single reads if the requests were of a similar
// nature, i.e. the whole file, of popular files.

// for pwrite/pread, I'm supposed to include the following three (osx man page)
// they don't appear to be necessary, but let's play it safe
#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>

namespace flx { namespace demux {
// fileio_request stuff follows

// read or write in a blocking fashion. I like the idea of using pread
// which doesn't change the file pointer. this could allow reuse of the same
// file descriptor & block caching

fileio_request::~fileio_request(){}
fileio_request::fileio_request(){}

fileio_request::fileio_request(int f, char* buf, long len, long off, bool rd)
  : offset(off), fd(f), read_flag(rd), err(0)
{
  pb.buffer = buf;
  pb.buffer_size = len;
  pb.bytes_written = 0;
}

// synchronously process read/write
void
fileio_request::doit()
{
  /*
  fprintf(stderr,"faio about to try to %s %i bytes from fd=%i\n",
    (read_flag) ? "read" : "write", pb.buffer_size, fd);
  */

// switching off (explicit) seeks for now because I'm not using them
// in the flx code & I'm not passing around enough info (just the fd)
  ssize_t res;

  if(read_flag)
  {
    // res = pread(fd, pb.buffer, pb.buffer_size, offset);
    res = read(fd, pb.buffer, pb.buffer_size);
  }
  else
  {
    // res = pwrite(fd, pb.buffer, pb.buffer_size, offset);
    res = write(fd, pb.buffer, pb.buffer_size);
  }

  // zero return value indicates end of file. that should just work.
  if(-1 == res)
  {
    err = errno;    // grab errno
    fprintf(stderr,"faio error: %i\n", err);
  }
  else
  {
    // fprintf(stderr,"faio %s %i bytes\n", (read_flag) ? "read" : "write", res);
    pb.bytes_written = res;
  }
}
}}

