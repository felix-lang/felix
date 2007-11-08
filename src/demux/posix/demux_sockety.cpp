#include "demux_sockety_config.hpp"
#include "demux_sockety.hpp" /* for this stuff */

#include <stdio.h>        /* for perror */
#include <fcntl.h>        /* for making non blocking sockets */
#include <netinet/in.h>     /* for sockaddr_in */
#include <arpa/inet.h>      /* for inet_addr */
#include <unistd.h>       /* for close */

#include <netinet/in.h>     /* IPPROTO_TCP and sockaddr_in */
#include <netinet/tcp.h>    /* TCP_NODELAY */

#include <sys/types.h>      /* for accept */
#include <sys/socket.h>
//#include <sys/errno.h>    /* EINPROGRESS (GUSI doesn't like both this and errno.h)*/

#include <errno.h>        /* errno */

#include <string.h>       /* for memset */

namespace flx { namespace demux {

/*
 returns a socket ready for listening (AF_INET, SOCK_STREAM for now).
 0 in for port means let kernel decide, result in *io_port
 portable, can be factored out. listens on all NICs.
 returns -1 on failure, sometimes eats errno.
 p.s. sets SO_REUSEADDR.
*/
int
create_listener_socket(int* io_port, int q_len)
{
  int         listener;
  int         yes = 1;  /* for address reuse */

  if((listener = socket(AF_INET, SOCK_STREAM, 0)) == -1)
    return -1;

  /* get rid of those *pesky* "address already in use" errors. */
  /* for when you don't cleanly shutdown the server */
  if(setsockopt(listener, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof(yes)) == -1)
    goto listener_fail;

  /* bind listener to local interface (ANY) */
  if(bind_sock(listener, io_port) == -1)
    goto listener_fail;

  /* set listen queue length for socket */
  if(listen(listener, q_len) == -1)
    goto listener_fail;

  return listener;

listener_fail:
  perror("create_listener_socket"); /* eats errno! */
  if(close(listener) == -1)
    perror("create_listener_socket close");
  return -1;
}

/* create listener, make it non-blocking */
/* duh, sometimes eats errno...*/
int
create_async_listener(int* io_port, int q_len)
{
  int   listener;

  listener = create_listener_socket(io_port, q_len);

  if(-1 == listener) return -1;

  if(make_nonblock(listener) == -1)
  {
    if(close(listener) != 0)
      perror("create_async_listener close");
    return -1;
  }

  return listener;
}

/* ps, sets resulting socket to non-block. some people would say that */
/* this WASN'T nice, so change the name some time. returns socket or -1 */
/* on failure, with *err containing the error code. on success returns s */
/* with zero (= no error) in *err */
int
nice_accept(int listener, int* err)
{
  struct sockaddr_in  remoteaddr;
  /*socklen_t     addrlen = sizeof(remoteaddr);*/
  /* os x 10.2.8 doesn't have socklen_t. will this work elsewhere? */
  /* 10.4 (gcc 4.0) complains about signedeness, so now unsigned */
  FLX_SOCKLEN_T addrlen = sizeof(remoteaddr);
  int         newfd;

  *err = 0;     /* assume all good */

  newfd = accept(listener, (struct sockaddr*)&remoteaddr, &addrlen);
  if(-1 == newfd) {
    *err = errno;
    return -1;
  }
  else
  {
    /*I think 0's the result I want*/
    if(make_nonblock(newfd) == -1)
    {
      *err = errno;

      /* bizarre case, note that close's errno is lost */
      if(close(newfd) == -1)
        perror("nice_accept can't set non-block");
      newfd = -1;
    }
    // Linger does nothing good in non-blocking mode
    else if(make_linger(newfd, 30) == -1)
    {
      *err = errno;

      // bizarre case, note that close's errno is lost
      if(close(newfd) == -1)
        perror("nice_accept can't set linger");
      newfd = -1;
    }
  }
  return newfd;
}

/* call this connect_ipv4? would its interface work for ipv6? */
/* this connect can be either asynchronous or synchronous, */
/* depending on whether or not the socket is non blocking */
/* returns -1 with err in errno on failure */
int
connect_sock(int s, const char* addr, int port)
{
  struct sockaddr_in  sock_addr;

  memset(&sock_addr, 0, sizeof(sock_addr));
  sock_addr.sin_family = AF_INET;     /* host byte order */
  sock_addr.sin_addr.s_addr = inet_addr(addr);
  sock_addr.sin_port = htons(port);

  return connect(s, (struct sockaddr *)&sock_addr, sizeof(sock_addr));
}

/* bind s to local address with given port number, or zero to let OS */
/* choose. can you bind to non-local addresses? not sure, but you might */
/* like to choose which local interface... (ADD OTHER INTERFACES HERE) */
/* returns -1 on failure with error code in errno */
int
bind_sock(int s, int* io_port)
{
  struct sockaddr_in  addr;
  FLX_SOCKLEN_T namelen = sizeof(addr);

  memset(&addr, 0, sizeof(addr));
  addr.sin_family = AF_INET;      /* host byte order */
  /* make the NIC an argument */
  addr.sin_addr.s_addr = htonl(INADDR_ANY); /* allow multihomed */
  addr.sin_port = htons(*io_port);

  /* bind to port */
  if (bind(s, (struct sockaddr *)&addr, namelen) < 0)
  {
    return -1;
  }

  /* we don't need to do this when the port was specified */
  if(0 == *io_port)
  {
    /* Find out what port number was chosen */
    if (getsockname(s, (struct sockaddr *)&addr, &namelen) < 0)
    {
      return -1;
    }

    *io_port = ntohs(addr.sin_port);
  }

  return 0; /* success! */
}

/* simple wrapper for fcntl for those too lazy to look it up */
/* returns -1 on failure with errno set or non -1  otherwise */
int
make_nonblock(int s)
{
  int old = fcntl(s, F_GETFL,0);
  if (old == -1) return old;
  return fcntl(s, F_SETFL, O_NONBLOCK | old );
}

int
make_linger(int s, int t)
{
  return 0;
  /*
  struct linger ling;
  ling.l_onoff = 1; // on
  ling.l_linger = t;
  return setsockopt(s, SOL_SOCKET, SO_LINGER,&ling, sizeof(ling));
  */
}


/* returns -1 on failure with errno set or 0 otherwise */
int
set_tcp_nodelay(int s, int disable_nagle)
{
  return setsockopt(s, IPPROTO_TCP, TCP_NODELAY,
    (char*)&disable_nagle, sizeof(disable_nagle));
}

/*
    Getting the determining if the async connect succeeded and if not,
    its error, can actually be quite hairy on some systems, see
    http://cr.yp.to/docs/connect.html
    for suggestions (none of which I follow at this point)
  returns 0 on success and socket error in *socket_err
  on failure returns -1 and *socket_err errno
*/
int
get_socket_error(int s, int* socket_err)
{
  int       res;
    FLX_SOCKLEN_T len = sizeof(*socket_err);
    /* god knows what the level should be. socket level seems sensible. */
    res = getsockopt(s, SOL_SOCKET, SO_ERROR, socket_err, &len);

    /* I've heard of impls of getsockopt(SO_ERROR) acting as they they */
    /* had the socket error (i.e. returning -1 and the sock err in errno) */
    if(-1 == res)
    {
        *socket_err = errno;     // don't think its ours
        fprintf(stderr, "getsockopt failed - is that our error? (%i)\n",
            *socket_err);
    }

  return res;
}

/* also make non-blocking AFTER connect, that is, */
/* this is a synchronous connect */
/* is eating errno, fix */
int
nice_connect(const char* addr, int port)
{
  int     s;

  if((s = socket(AF_INET, SOCK_STREAM, 0)) != -1
    && connect_sock(s, addr, port) == 0
    && make_nonblock(s) != -1)
  {
    return s;   /* success! */
  }

  /* something happened (not as good as catch 22) */
  perror("nice_connect");

  if(-1 != s && close(s) != 0)
    perror("nice close");

  return -1;
}

/* makes the socket non-blocking BEFORE connect, returns result */
/* from which can be determined if it finished immediately */
/* returns the socket & finished flag or -1 on failure, with the */
/* error returned in *err */
int
async_connect(const char* addr, int port, int* finished, int* err)
{
  int     s = -1;

  if((s = socket(AF_INET, SOCK_STREAM, 0)) != -1 && make_nonblock(s) != -1)
  {
    /* no error we now have s, a non-blocking socket */
    if(connect_sock(s, addr, port) == 0)
    {
      *err = 0;         /* no error */
      *finished = 1;        /* finished */
      return s;
    }

    *err = errno;         /* connect failed or in-progress */

    /* this can apparently be EWOULDBLOCK or even EAGAIN on some systems */
    /* any info? on some systems they're the same, on some they're not */
    if(EINPROGRESS == *err)
    {
      *finished = 0;        /* not finished, in progress */
      return s;
    }
    /* some other failure, fall through and clean up */
  }

  /* hope you can read same errno twice in threaded apps! */
  *err = errno;           /* pass back error */

  if(-1 != s && close(s) != 0)    /* we lose the close error */
    perror("async_connect close");

  *finished = 1;            /* for completeness */
  return -1;
}
}}
