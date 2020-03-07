#ifndef __FLX_DEMUX_WIN_TIMER_QUEUE_H__
#define __FLX_DEMUX_WIN_TIMER_QUEUE_H__

#include "flx_demux_config.hpp"
#include <Windows.h>

#include "demux_timer_queue.hpp"

namespace flx { namespace demux {

class DEMUX_EXTERN win_timer_queue : public timer_queue
{
  HANDLE    timer_queue;

  static VOID CALLBACK timer_callback(PVOID, BOOLEAN);
public:
  win_timer_queue();
  ~win_timer_queue();

  virtual void add_sleep_request(sleep_task* st, double delta);
  virtual void add_abs_sleep_request(sleep_task* st, double when);

};

}}

#endif // __SLEEP_TASK__
