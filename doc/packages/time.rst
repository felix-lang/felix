
===========
Time of Day
===========



RTC: Time of Day
================

A Real Time Clock (RTC) is a device that provides the
current date and time of day.

.. code-block:: felix

   class Time_class [os] {
     virtual gen time: 1 -> double; // time in seconds since Jan 1 1970 UTC, seconds
   }
   
   open class Time {
   if PLAT_WIN32 do
     inherit Win32Time;
   else
     inherit PosixTime;
   done
   }
   

Posix RTC
=========


.. code-block:: felix

   
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


.. code-block:: felix

   
   class Win32Time
   {
     requires Posix_headers::sys_types_h;
     requires Win32_headers::sys_timeb_h;
   
     private type time_t = "time_t";
     private fun _ctor_double: time_t -> double = "static_cast<double>($1)";
   
     private cstruct __timeb64 {
       time: time_t; // seconds
       millitm: ushort; // milliseconds
     };
   
     private proc _ftime64_s: &__timeb64 = "_ftime64_s($1);";
   
     inherit Time_class[Win32];
   
     instance Time_class[Win32] {
       gen time () : double = {
         var tv:__timeb64;
         _ftime64_s(&tv);
         return tv.time.double + tv.millitm.double / 1.0e3;
       }
     }
   }
   