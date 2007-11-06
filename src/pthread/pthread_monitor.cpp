#include "pthread_monitor.hpp"
#include <queue>        // stl to the bloated rescue
#include <string.h>       // strerror
#include <assert.h>

using namespace std;

namespace flx { namespace pthread {

monitor_t::monitor_t() : dataput(0),datagot(0) {}
monitor_t::~monitor_t() { }
inline static void handshake_pos(int &a, flx_condv_t &c, flx_mutex_t &m)
{
  ++a;
  if(a != 0) do { c.wait(&m); } while (a != 0);
  else c.signal();
  assert(a == 0);
  //if(!(a == 0)) fprintf(stderr,"ASSER FAIL\n");
}

inline static void handshake_neg(int &a, flx_condv_t &c, flx_mutex_t &m)
{
  --a;
  if(a != 0) do { c.wait(&m); } while (a != 0);
  else c.signal();
  assert(a == 0);
  //if(!(a == 0)) fprintf(stderr,"ASSER FAIL\n");
}

void
monitor_t::enqueue(void* elt)
{
  flx_mutex_locker_t   wl(wm); // exclude other writers
  flx_mutex_locker_t   l(m);
  data = elt;
  handshake_pos(dataput, ack, m);
  handshake_pos(datagot, ack, m);
}

void*
monitor_t::dequeue()
{
  flx_mutex_locker_t   rl(rm); // exclude other readers
  flx_mutex_locker_t   l(m);
  handshake_neg(dataput, ack, m);
  void *d = data;              // get the data
  handshake_neg(datagot, ack, m);
  return d;
}

}}

