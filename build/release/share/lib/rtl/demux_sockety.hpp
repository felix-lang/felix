#ifndef __FLX_DEMUX_SOCKETY_H__
#define __FLX_DEMUX_SOCKETY_H__
#include <flx_demux_config.hpp>
namespace flx { namespace demux {

DEMUX_EXTERN int create_listener_socket(int* io_port, int q_len);
DEMUX_EXTERN int create_async_listener(int* io_port, int q_len);
DEMUX_EXTERN int nice_accept(int listener, int* err);
DEMUX_EXTERN int nice_connect(const char* addr, int port);
DEMUX_EXTERN int async_connect(const char* addr, int port, int* finished, int* err);

/* handy socket building blocks */

DEMUX_EXTERN int connect_sock(int s, const char* addr, int port);

/* this could possibly do with NIC addr as well as port */
DEMUX_EXTERN int bind_sock(int s, int* io_port);

DEMUX_EXTERN int make_nonblock(int s);
DEMUX_EXTERN int make_linger(int s, int t);
DEMUX_EXTERN int set_tcp_nodelay(int s, int disable_nagle);
DEMUX_EXTERN int get_socket_error(int s, int* socket_err);

}} // namespace demux, flx
#endif
