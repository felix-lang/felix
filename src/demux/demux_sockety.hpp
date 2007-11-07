#line 607 "../lpsrc/flx_posix_demux.ipk"
#ifndef __FLX_DEMUX_SOCKETY_H__
#define __FLX_DEMUX_SOCKETY_H__
#include <flx_demux_config.hpp>
namespace flx { namespace demux {

// Shouldn't this all be DEMUX_EXTERN? eh, not actually compiled on win32
// we'll probably live.
int create_listener_socket(int* io_port, int q_len);
int create_async_listener(int* io_port, int q_len);
int nice_accept(int listener, int* err);
int nice_connect(const char* addr, int port);
int async_connect(const char* addr, int port, int* finished, int* err);

/* handy socket building blocks */

int connect_sock(int s, const char* addr, int port);

/* this could possibly do with NIC addr as well as port */
int bind_sock(int s, int* io_port);

int make_nonblock(int s);
int make_linger(int s, int t);
int set_tcp_nodelay(int s, int disable_nagle);
int get_socket_error(int s, int* socket_err);

}} // namespace demux, flx
#endif

