#ifndef __FLX_DEMUX_TIMER_QUEUE_H__
#define __FLX_DEMUX_TIMER_QUEUE_H__

#include <flx_demux_config.hpp>

namespace flx { namespace demux {
class DEMUX_EXTERN sleep_task
{
public:
    virtual ~sleep_task() {}

    virtual void fire() = 0;
};

class DEMUX_EXTERN timer_queue
{
public:
    virtual ~timer_queue() {}

    virtual void add_sleep_request(sleep_task* st, double delta) = 0;
    virtual void add_abs_sleep_request(sleep_task* st, double when) = 0;

    // bad design - this is actually implemented in the descendent classes,
    // which limits the number of such classes probably to one.
    static void get_time(double& t);        // in seconds from some ref pt
};

DEMUX_EXTERN timer_queue *mk_timer_queue();

}} // namespace demux, flx

#endif
