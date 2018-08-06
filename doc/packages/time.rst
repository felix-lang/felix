Package: src/packages/time.fdoc


===========
Time of Day
===========

============== ============================
key            file                         
============== ============================
time.flx       share/lib/std/time.flx       
posix_time.flx share/lib/std/posix/time.flx 
win32_time.flx share/lib/std/win32/time.flx 
============== ============================


RTC: Time of Day
================

A Real Time Clock (RTC) is a device that provides the
current date and time of day.


.. index:: Time_class
.. index:: Time
.. code-block:: felix

  //[time.flx]
  class Time_class [os] {
    virtual gen time: 1 -> double; // time in seconds since Jan 1 1970 UTC, seconds
  }
  
  open class Time {
  if PLAT_WIN32 do
    inherit Win32Time;
  else
    inherit PosixTime;
  done
    rename fun sleep =  Faio::sleep; 
  }
  
Posix RTC
=========



.. index:: PosixTime
.. code-block:: felix

  //[posix_time.flx]
  
  class PosixTime
  {
    requires Posix_headers::sys_time_h;
  
    private type time_t = "time_t";
    private type suseconds_t = "suseconds_t";
  
    private fun _ctor_double: time_t -> double = "static_cast<double>($1)";
    private fun _ctor_double: suseconds_t -> double = "static_cast<double>($1)";
  
    private cstruct timeval {
      tv_sec: time_t;
      tv_usec: suseconds_t;
    };
  
    private proc gettimeofday: &timeval = "gettimeofday($1, NULL);";
  
    inherit Time_class[Posix];
  
    instance Time_class[Posix] {
      gen time () : double = {
        var tv:timeval;
        gettimeofday(&tv);
        return tv.tv_sec.double + tv.tv_usec.double / 1.0e6;
      }
    }
  }
  
Win32 RTC
=========


