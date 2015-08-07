#include "pthread_thread.hpp"
#include <stdio.h>
#include <cstdlib>
#include <cassert>

#define FLX_SAVE_REGS \
  jmp_buf reg_save_on_stack; \
  setjmp (reg_save_on_stack)


namespace flx { namespace pthread {

static void *get_stack_pointer() { void *x = (void*)&x; return x; }

thread_control_t::thread_control_t (thread_control_t const &) {}
void thread_control_t::operator=(thread_control_t const &) {}

thread_control_t::thread_control_t (bool d) :
  do_world_stop(false), thread_counter(0), active_counter(0), debug(d)
  {
    if(debug)
      fprintf(stderr,"INITIALISING THREAD CONTROL OBJECT\n");
  }

int thread_control_t::thread_count()
  {
    flx_mutex_locker_t m(stop_mutex);
    return thread_counter;
  }

int thread_control_t::active_count()
  {
    flx_mutex_locker_t m(stop_mutex);
    return active_counter;
  }

void thread_control_t::add_thread(void *stack_base)
  {
    flx_mutex_locker_t m(stop_mutex);
    flx_native_thread_t id = get_current_native_thread();
    threads.insert (std::make_pair(id, thread_data_t (stack_base)));
    ++thread_counter;
    ++active_counter;
    if(debug)
      fprintf(stderr, "Adding thread %lx base %p, count=%d\n", (unsigned long)id, stack_base, thread_counter);
    stop_guard.broadcast();
  }

void thread_control_t::remove_thread()
  {
    flx_mutex_locker_t m(stop_mutex);
    flx_native_thread_t id = get_current_native_thread();
    if (threads.erase(id) == 0)
    {
      fprintf(stderr, "Remove thread %lx which is not registered\n", (unsigned long)id);
      std::abort();
    }
    --thread_counter;
    --active_counter;
    if(debug)
      fprintf(stderr, "Removed thread %lx, count=%d\n", (unsigned long)id, thread_counter);
    stop_guard.broadcast();
  }

// stop the world!

// NOTE: ON EXIT, THE MUTEX REMAINS LOCKED

bool thread_control_t::world_stop()
  {
    stop_mutex.lock();
    if(debug)
      fprintf(stderr,"Thread %lx Stopping world, active threads=%d\n", (unsigned long)get_current_native_thread(), active_counter);
    if (do_world_stop) {
      stop_mutex.unlock();
      return false; // race! Someone else beat us
    }
    do_world_stop = true;
    stop_guard.broadcast();
    while(active_counter>1) {
      if(debug)
        for(
          thread_registry_t::iterator it = threads.begin();
          it != threads.end();
          ++it
        )
        {
          fprintf(stderr, "Thread = %lx is %s\n",(unsigned long)(*it).first, (*it).second.active? "ACTIVE": "SUSPENDED");
        }
      if(debug)
        fprintf(stderr,"Thread %lx Stopping world: begin wait, threads=%d\n",  (unsigned long)get_current_native_thread(), thread_counter);
      stop_guard.wait(&stop_mutex);
      if(debug)
        fprintf(stderr,"Thread %lx Stopping world: checking threads=%d\n", (unsigned long)get_current_native_thread(), thread_counter);
    }
    // this code has to be copied here, we cannot use 'yield' because
    // it would deadlock ourself
    {
      flx_native_thread_t id = get_current_native_thread();
      FLX_SAVE_REGS;
      void *stack_pointer = get_stack_pointer();
      if(debug)
        fprintf(stderr,"World stop thread=%lx, stack=%p!\n",(unsigned long)id, stack_pointer);
      thread_registry_t::iterator it = threads.find(id);
      if(it == threads.end()) {
        fprintf(stderr,"MAIN THREAD: Cannot find thread %lx in registry\n",(unsigned long)id);
        abort();
      }
      (*it).second.stack_top = stack_pointer;
      if(debug)
        fprintf(stderr,"Stack size = %ld\n",(long)((char*)(*it).second.stack_base -(char*)(*it).second.stack_top));
    }
    if(debug)
      fprintf(stderr,"World STOPPED\n");
    return true; // we stopped the world
  }

// used by mainline to wait for other threads to die
void thread_control_t::join_all()
  {
    flx_mutex_locker_t m(stop_mutex);
    if(debug)
      fprintf(stderr,"Thread %lx Joining all\n", (unsigned long)get_current_native_thread());
    while(do_world_stop || thread_counter>1) {
      unsafe_stop_check();
      stop_guard.wait(&stop_mutex);
    }
    if(debug)
      fprintf(stderr,"World restarted: do_world_stop=%d, Yield thread count now %d\n",do_world_stop,thread_counter);
  }

// restart the world
void thread_control_t::world_start()
  {
    if(debug)
      fprintf(stderr,"Thread %lx Restarting world\n", (unsigned long)get_current_native_thread());
    do_world_stop = false;
    stop_mutex.unlock();
    stop_guard.broadcast();
  }

memory_ranges_t *thread_control_t::get_block_list()
{
  memory_ranges_t *v = new std::vector<memory_range_t>;
  thread_registry_t::iterator end = threads.end();
  for(thread_registry_t::iterator i = threads.begin();
    i != end;
    ++i
  )
  {
    thread_data_t const &td = (*i).second;
    // !(base < top) means top <= base, i.e. stack grows downwards
    assert(!std::less<void*>()(td.stack_base,td.stack_top));
    // from top upto base..
    v->push_back(memory_range_t(td.stack_top, td.stack_base));
  }
  return v;
}

void thread_control_t::suspend()
{
  flx_mutex_locker_t m(stop_mutex);
  if(debug)
    fprintf(stderr,"suspend %lx\n", (unsigned long)get_current_native_thread());
  unsafe_suspend();
}

void thread_control_t::resume()
{
  flx_mutex_locker_t m(stop_mutex);
  if(debug)
    fprintf(stderr,"resume %lx\n", (unsigned long)get_current_native_thread());
  unsafe_resume();
}


void thread_control_t::unsafe_suspend()
{
  void *stack_pointer = get_stack_pointer();
  flx_native_thread_t id = get_current_native_thread();
  if(debug)
    fprintf(stderr,"Suspend found world stop, thread=%lx, stack=%p!\n",(unsigned long)id, stack_pointer);
  thread_registry_t::iterator it = threads.find(id);
  if(it == threads.end()) {
    if(debug)
      fprintf(stderr,"Suspend::Cannot find thread %lx in registry\n",(unsigned long)id);
      abort();
  }
  (*it).second.stack_top = stack_pointer;
  (*it).second.active = false;
  if(debug)
    fprintf(stderr,"Stack size = %ld\n",(long)((char*)(*it).second.stack_base -(char*)(*it).second.stack_top));
  --active_counter;
  if(debug)
    fprintf(stderr,"Suspend: active thread count now %d\n",active_counter);
  stop_guard.broadcast();
}

void thread_control_t::unsafe_resume()
{
  if(debug)
    fprintf(stderr,"Unsafe resume %lx\n", (unsigned long)get_current_native_thread());
  stop_guard.broadcast();
  while(do_world_stop) stop_guard.wait(&stop_mutex);
  ++active_counter;
  flx_native_thread_t id = get_current_native_thread();
  thread_registry_t::iterator it = threads.find(id);
  if(it == threads.end()) {
    if(debug)
      fprintf(stderr,"Suspend::Cannot find thread %lx in registry\n",(unsigned long)id);
      abort();
  }
  (*it).second.active = true;
  if(debug) {
    fprintf(stderr,"Thread %lx resumed, active count= %d\n",(unsigned long)get_current_native_thread(),active_counter);
  }
  stop_guard.broadcast();
}

// mutex already held
void thread_control_t::unsafe_stop_check()
{
  if (do_world_stop)
  {
    if(debug)
      fprintf(stderr,"Thread %lx Unsafe stop check (world_stop detected) \n", (unsigned long)get_current_native_thread());
    FLX_SAVE_REGS;
    unsafe_suspend();
    unsafe_resume();
  }
}

void thread_control_t::yield()
{
  flx_mutex_locker_t m(stop_mutex);
  if(debug)
    fprintf(stderr,"Yield %lx\n", (unsigned long)get_current_native_thread());
  unsafe_stop_check();
}

}}

