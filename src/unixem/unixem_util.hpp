#ifndef __UNIXEM_UTIL_
#define __UNIXEM_UTIL_

int errno_from_Win32(unsigned long w32Err);
char get_current_drive(void);
# define UNIXEM_NUM_ELEMENTS(x)        (sizeof(x) / sizeof(0[x]))
#endif
