/*
@(#)File:           $RCSfile: stderr.h,v $
@(#)Version:        $Revision: 10.1 $
@(#)Last changed:   $Date: 2010/09/25 18:12:21 $
@(#)Purpose:        Header file for standard error functions
@(#)Author:         J Leffler
@(#)Copyright:      (C) JLSS 1989-93,1996-99,2003,2005-10
@(#)Product:        TIMEOUT Version 4.11 (2010-12-04)
*/

#ifndef STDERR_H
#define STDERR_H

#ifdef MAIN_PROGRAM
#ifndef lint
/* Prevent over-aggressive optimizers from eliminating ID string */
const char jlss_id_stderr_h[] = "@(#)$Id: stderr.h,v 10.1 2010/09/25 18:12:21 jleffler Exp $";
#endif /* lint */
#endif

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdarg.h>

#ifdef __GNUC__
#define PRINTFLIKE(n,m) __attribute__((format(printf,n,m)))
#define NORETURN()      __attribute__((noreturn))
#else
#define PRINTFLIKE(n,m) /* If only */
#define NORETURN()      /* If only */
#endif /* __GNUC__ */

/* -- Definitions for error handling */

enum { ERR_STAT    = 1 };           /* Default exit status     */

enum { ERR_DEFAULT = 0x0000 };      /* Default flag             */
enum { ERR_NOFLUSH = 0x0001 };      /* Do not flush open files  */
enum { ERR_EXIT    = 0x0004 };      /* Exit  -- do not return   */
enum { ERR_ABORT   = 0x0008 };      /* Abort -- do not return   */
enum { ERR_STAMP   = 0x0020 };      /* Timestamp messages       */
enum { ERR_NOARG0  = 0x0040 };      /* Do not print arg0 prefix */
enum { ERR_PID     = 0x0080 };      /* Include pid=nnnnn info   */
enum { ERR_ERRNO   = 0x0100 };      /* Include system error     */

#ifdef USE_STDERR_SYSLOG
/* Definitions related to using syslog */
enum { ERR_LOG_EMERG    = 0x01000 };    /* system is unusable */
enum { ERR_LOG_ALERT    = 0x02000 };    /* action must be taken immediately */
enum { ERR_LOG_CRIT     = 0x04000 };    /* critical conditions */
enum { ERR_LOG_ERR      = 0x08000 };    /* error conditions */
enum { ERR_LOG_WARNING  = 0x10000 };    /* warning conditions */
enum { ERR_LOG_NOTICE   = 0x20000 };    /* normal but signification condition */
enum { ERR_LOG_INFO     = 0x40000 };    /* informational */
enum { ERR_LOG_DEBUG    = 0x80000 };    /* debug-level messages */
enum { ERR_LOG_LEVEL_HI = ERR_LOG_EMERG|ERR_LOG_ALERT|ERR_LOG_CRIT|ERR_LOG_ERR };
enum { ERR_LOG_LEVEL_LO = ERR_LOG_WARNING|ERR_LOG_NOTICE|ERR_LOG_INFO|ERR_LOG_DEBUG };
enum { ERR_LOG_LEVEL    = ERR_LOG_LEVEL_HI|ERR_LOG_LEVEL_LO };
#endif /* USE_STDERR_SYSLOG */

/* -- Standard combinations of flags */

enum { ERR_REM    = ERR_DEFAULT       };
enum { ERR_ERR    = ERR_EXIT          };
enum { ERR_ABT    = ERR_ABORT         };
enum { ERR_LOG    = ERR_STAMP|ERR_PID };
enum { ERR_SYSREM = ERR_REM|ERR_ERRNO };
enum { ERR_SYSERR = ERR_ERR|ERR_ERRNO };

/* -- Maximum recorded length of argv[0]; extra is truncated */

enum { ERR_MAXLEN_ARGV0 = 63 };

/* -- Global definitions */

extern const char  err_format1[];    /* "%s\n"    - for one string argument */
extern const char  err_format2[];    /* "%s %s\n" - for two string arguments */

extern const char *err_getarg0(void);
extern void        err_setarg0(const char *argv0);

extern FILE       *err_stderr(FILE *fp);
extern const char *err_rcs_string(const char *s, char *buffer, size_t buflen);

extern void err_abort(const char *format, ...) PRINTFLIKE(1,2) NORETURN();
extern void err_error(const char *format, ...) PRINTFLIKE(1,2) NORETURN();
extern void err_error1(const char *s1) NORETURN();
extern void err_error2(const char *s1, const char *s2) NORETURN();
extern void err_help(const char *use_str, const char *hlp_str) NORETURN();
extern void err_helplist(const char *use_str, const char * const *help_list) NORETURN();
extern void err_internal(const char *function, const char *format, ...) PRINTFLIKE(2,3) NORETURN();
extern void err_logmsg(FILE *fp, int flags, int estat, const char *format, ...) PRINTFLIKE(4,5);
extern void err_print(int flags, int estat, const char *format, va_list args);
extern void err_printversion(const char *program, const char *verinfo);
extern void err_remark(const char *format, ...) PRINTFLIKE(1,2);
extern void err_remark1(const char *s1);
extern void err_remark2(const char *s1, const char *s2);
extern void err_report(int flags, int estat, const char *format, ...) PRINTFLIKE(3,4);
extern void err_syserr(const char *format, ...) PRINTFLIKE(1,2) NORETURN();
extern void err_syserr1(const char *s1) NORETURN();
extern void err_syserr2(const char *s1, const char *s2) NORETURN();
extern void err_sysrem(const char *format, ...) PRINTFLIKE(1,2);
extern void err_sysrem1(const char *s1);
extern void err_sysrem2(const char *s1, const char *s2);
extern void err_usage(const char *usestr) NORETURN();
extern void err_version(const char *program, const char *verinfo) NORETURN();

extern int  err_getlogopts(void);           /* Get default log options */
extern int  err_setlogopts(int new_opts);   /* Set default log options */

#ifdef USE_STDERR_FILEDESC
extern int  err_use_fd(int fd);             /* Use file descriptor */
#endif /* USE_STDERR_FILEDESC */
#ifdef USE_STDERR_SYSLOG
/* In case of doubt, use zero for both logopts and facility */
extern int  err_use_syslog(int logopts, int facility);  /* Configure/use syslog() */
#endif /* USE_STDERR_SYSLOG */

/*
** JL 2003-07-31: Security Note.
** Question: given that err_remark("abc\n") and err_remark1("abc")
**           produce the same output, when should you use err_remark1()
**           instead of err_remark()?
** Answer 1: trivia - when you can't put the newline in the string.
** Answer 2: security - when the argument contains user input and could,
**           therefore, contain conversion specifiers, etc.  The code in
**           err_remark() does not (and cannot) verify that you have
**           passed correct arguments for the conversion specifiers in
**           the format string.
** Answer 3: inertia - when migrating code that uses remark().
**
** Generalizing: when you use a function that has 'const char *format'
** in the prototype above, make sure your code is fully in charge of the
** format string to avoid security lapses.  Do not allow the user to
** provide that string unless you stringently check it beforehand.
*/

#endif /* STDERR_H */
