/*
@(#)File:           $RCSfile: stderr.c,v $
@(#)Version:        $Revision: 10.3 $
@(#)Last changed:   $Date: 2010/10/17 22:51:27 $
@(#)Purpose:        Error reporting routines
@(#)Author:         J Leffler
@(#)Copyright:      (C) JLSS 1988-91,1996-99,2001,2003,2005-10
@(#)Product:        TIMEOUT Version 4.11 (2010-12-04)
*/

/*TABSTOP=4*/

/*
** Configuration:
** USE_STDERR_SYSLOG   - include syslog functionality
** USE_STDERR_FILEDESC - include file descriptor functionality
*/

#ifdef JLSS_STDERR
#define USE_STDERR_FILEDESC
#define USE_STDERR_SYSLOG
#endif /* JLSS_STDERR */

#include "stderr.h"

#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <time.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#else
extern int getpid(void);
#endif /* HAVE_UNISTD_H */

enum { MAX_MSGLEN = 2048 };

#if defined(HAVE_SYSLOG_H) && defined(HAVE_SYSLOG) && defined(USE_STDERR_SYSLOG)
#include <syslog.h>
const char jlss_id_stderr_c_with_syslog[] =
        "@(#)" __FILE__ " configured with USE_STDERR_SYSLOG";
#else
#undef USE_STDERR_SYSLOG
#undef HAVE_SYSLOG_H
#undef HAVE_SYSLOG
#endif /* syslog configuration */

#if defined(USE_STDERR_FILEDESC)
const char jlss_id_stderr_c_with_filedesc[] =
        "@(#)" __FILE__ " configured with USE_STDERR_FILEDESC";
#endif /* USE_STDERR_FILEDESC */

/* Global format strings */
const char err_format1[] = "%s\n";
const char err_format2[] = "%s %s\n";

static const char def_format[] = "%Y-%m-%d %H:%M:%S";
static const char *tm_format = def_format;
static char arg0[ERR_MAXLEN_ARGV0+1] = "**undefined**";

/* Permitted default error flags */
enum { ERR_LOGOPTS = ERR_NOFLUSH | ERR_EXIT | ERR_ABORT | ERR_STAMP |
                     ERR_NOARG0  | ERR_PID  | ERR_ERRNO };
static int   err_flags = 0;     /* Default error flags (ERR_STAMP, ERR_PID, etc) */

/* Where do messages go?
**  if   (defined USE_STDERR_SYSLOG && errlog != 0) ==> syslog
**  elif (err_fd >= 0)                       ==> file descriptor
**  else                                     ==> file pointer
*/
static int   errlog =  0;
static FILE *errout =  0;
static int   err_fd = -1;

/*
** err_???_print() functions are named systematically, and are all static.
**
** err_[ev][crx][fn]_print():
** --   e   takes ellipsis argument
** --   v   takes va_list argument
** --   c   conditionally exits
** --   r   returns (no exit)
** --   x   exits (no return)
** --   f   takes file pointer
** --   n   no file pointer (use errout)
**
** NB: no-return and printf-like can only be attached to declarations, not definitions.
*/

static void err_vxf_print(FILE *fp, int flags, int estat, const char *format, va_list args)
                NORETURN();
static void err_vxn_print(int flags, int estat, const char *format, va_list args)
                NORETURN();
static void err_exn_print(int flags, int estat, const char *format, ...)
                NORETURN() PRINTFLIKE(3,4);
static void err_terminate(int flags, int estat) NORETURN();

#ifndef lint
/* Prevent over-aggressive optimizers from eliminating ID string */
const char jlss_id_stderr_c[] = "@(#)$Id: stderr.c,v 10.3 2010/10/17 22:51:27 jleffler Exp $";
#endif /* lint */

/*
** Set default log options, returning old value.
** Setting ERR_EXIT and ERR_ABORT is permitted but not recommended.
*/
int err_setlogopts(int new_opts)
{
    int old_opts = err_flags;
    err_flags = new_opts & ERR_LOGOPTS;
    return(old_opts);
}

/* Return default log options */
int err_getlogopts(void)
{
    return(err_flags);
}

/* Change the definition of 'stderr', reporting on the old one too */
/* NB: using err_stderr((FILE *)0) simply reports the current 'stderr' */
FILE *(err_stderr)(FILE *newerr)
{
    FILE *old;

    if (errout == 0)
        errout = stderr;
    old = errout;
    if (newerr != 0)
        errout = newerr;
    return(old);
}

/* Change the definition of 'stderr', reporting on the old one too */
/* NB: using err_use_fd() with a negative value turns off 'errors to file descriptor' */
int (err_use_fd)(int new_fd)
{
    int old_fd = err_fd;

#if defined(USE_STDERR_FILEDESC)
    if (new_fd < 0)
        new_fd = -1;
    err_fd = new_fd;
#endif /* USE_STDERR_FILEDESC */
    return(old_fd);
}

/*
** Configure the use of syslog
** If not configured to use syslog(), this is a no-op.
** If configured to use syslog(), the facility argument should be one of
** the standard facilities (POSIX defines LOG_USER and LOG_LOCAL0 to
** LOG_LOCAL7) to turn on syslog(), or a negative value to turn it off.
** The logopts should be the bitwise combination of 0 and the options
** LOG_PID, LOG_CONS, LOG_NDELAY, LOG_ODELAY, LOG_NOWAIT.  However, the
** STDERR package sets LOG_PID regardless.
** The ident used in openlog() corresponds to the value in arg0.
** Note that when formatting the message for syslog(), the time, the PID
** and arg0 are not needed (and hence not provided).  The downside is
** you are stuck with the date formatted by syslog().
*/
int (err_use_syslog)(int logopts, int facility)
{
#if defined(USE_STDERR_SYSLOG)
    if (facility < 0)
    {
        /* Turn off syslog() */
        closelog();
        errlog = 0;
    }
    else
    {
        openlog(arg0, LOG_PID|logopts, facility);
        errlog = 1;
    }
#endif /* USE_STDERR_SYSLOG */
    return(errlog);
}

/* Return stored basename of command */
const char *(err_getarg0)(void)
{
    return(arg0);
}

/* Store basename of command, excluding trailing slashes */
void (err_setarg0)(const char *argv0)
{
    /* Ignore three pathological program names -- NULL, "/" and "" */
    if (argv0 != 0 && *argv0 != '\0' && (*argv0 != '/' || *(argv0 + 1) != '\0'))
    {
        const char *cp;
        size_t nbytes = sizeof(arg0) - 1;

        if ((cp = strrchr(argv0, '/')) == 0)
        {
            /* Basename of file only */
            cp = argv0;
        }
        else if (*(cp + 1) != '\0')
        {
            /* Regular pathname containing slashes but not trailing slashes */
            cp++;
        }
        else
        {
            /* Skip backwards over trailing slashes */
            const char *ep = cp;
            while (ep > argv0 && *ep == '/')
                ep--;
            /* Skip backwards over non-slashes */
            cp = ep;
            while (cp > argv0 && *cp != '/')
                cp--;
            assert(ep >= cp);
            cp++;
            nbytes = (size_t)(ep - cp) + 1;
            if (nbytes > sizeof(arg0) - 1)
                nbytes = sizeof(arg0) - 1;
        }
        strncpy(arg0, cp, nbytes);
        arg0[nbytes] = '\0';
    }
}

const char *(err_rcs_string)(const char *s2, char *buffer, size_t buflen)
{
    const char *src = s2;
    char *dst = buffer;
    char *end = buffer + buflen - 1;

    /*
    ** Bother RCS!  We've probably been given something like:
    ** "$Revision: 10.3 $ ($Date: 2010/10/17 22:51:27 $)"
    ** We only want to emit "7.5 (2001/08/11 06:25:48)".
    ** Skip the components between '$' and ': ', copy up to ' $',
    ** repeating as necessary.  And we have to test for overflow!
    ** Also work with the unexpanded forms of keywords ($Keyword$).
    ** Never needed this with SCCS!
    */
    while (*src != '\0' && dst < end)
    {
        while (*src != '\0' && *src != '$')
        {
            *dst++ = *src++;
            if (dst >= end)
                break;
        }
        if (*src == '$')
            src++;
        while (*src != '\0' && *src != ':' && *src != '$')
            src++;
        if (*src == '\0')
            break;
        if (*src == '$')
        {
            /* Unexpanded keyword '$Keyword$' notation */
            src++;
            continue;
        }
        if (*src == ':')
            src++;
        if (*src == ' ')
            src++;
        while (*src != '\0' && *src != '$')
        {
            /* Map / in 2009/02/15 to dash */
            /* Heuristic - maps slashes surrounded by digits to dashes */
            char c = *src++;
            if (c == '/' && isdigit(*src) && isdigit(*(src-2)))
                c = '-';
            *dst++ = c;
            if (dst >= end)
                break;
        }
        if (*src == '$')
        {
            if (*(dst-1) == ' ')
                dst--;
            src++;
        }
    }
    *dst = '\0';
    return(buffer);
}

/* Format a time string for now (using ISO8601 format) */
/* Allow for future settable time format with tm_format */
static char *err_time(char *buffer, size_t buflen)
{
    time_t  now;
    struct tm *tp;

    now = time((time_t *)0);
    tp = localtime(&now);
    strftime(buffer, buflen, tm_format, tp);
    return(buffer);
}

/* err_stdio - report error via stdio */
static void (err_stdio)(FILE *fp, int flags, int errnum, const char *format, va_list args)
{
    if ((flags & ERR_NOARG0) == 0)
        fprintf(fp, "%s: ", arg0);
    if (flags & ERR_STAMP)
    {
        char timbuf[32];
        fprintf(fp, "%s - ", err_time(timbuf, sizeof(timbuf)));
    }
    if (flags & ERR_PID)
        fprintf(fp, "pid=%d: ", (int)getpid());
    vfprintf(fp, format, args);
    if (flags & ERR_ERRNO)
        fprintf(fp, "error (%d) %s\n", errnum, strerror(errnum));
}

#if defined(USE_STDERR_FILEDESC) || defined(USE_STDERR_SYSLOG)
static char *fmt_string(char *curr, const char *end, const char *format, va_list args)
{
    char *new_end = curr;
    if (curr < end - 1)
    {
        int size = end - curr;
        int more;
        more = vsnprintf(curr, end - curr, format, args);
        if (more >= 0)
            new_end += (more >= size) ? size : more;
    }
    return(new_end);
}

static char *fmt_strdots(char *curr, const char *end, const char *format, ...)
{
    va_list args;
    char *new_end;
    va_start(args, format);
    new_end = fmt_string(curr, end, format, args);
    va_end(args);
    return new_end;
}
#endif /* USE_STDERR_FILEDESC || USE_STDERR_SYSLOG */

#if defined(USE_STDERR_SYSLOG)
/* err_syslog() - report error via syslog
**
** syslog() automatically adds PID and program name (configured in openlog()) and time stamp.
*/
static void (err_syslog)(int flags, int errnum, const char *format, va_list args)
{
    char buffer[MAX_MSGLEN];
    char *curpos = buffer;
    char *bufend = buffer + sizeof(buffer);
    int priority;

    curpos = fmt_string(curpos, bufend, format, args);
    if (flags & ERR_ERRNO)
        curpos = fmt_strdots(curpos, bufend,
                            "error (%d) %s\n", errnum, strerror(errnum));

    if (flags & ERR_ABORT)
        priority = LOG_CRIT;
    else if (flags & ERR_EXIT)
        priority = LOG_ERR;
    else
        priority = LOG_WARNING;
    syslog(priority, "%s", buffer);
}
#endif /* USE_STDERR_SYSLOG */

#if defined(USE_STDERR_FILEDESC)
/* err_filedes() - report error via file descriptor */
static void (err_filedes)(int fd, int flags, int errnum, const char *format, va_list args)
{
    char buffer[MAX_MSGLEN];
    char *curpos = buffer;
    char *bufend = buffer + sizeof(buffer);

    buffer[0] = '\0';   /* Not strictly necessary */
    if ((flags & ERR_NOARG0) == 0)
        curpos = fmt_strdots(curpos, bufend, "%s: ", arg0);
    if (flags & ERR_STAMP)
    {
        char timbuf[32];
        curpos = fmt_strdots(curpos, bufend,
                             "%s - ", err_time(timbuf, sizeof(timbuf)));
    }
    if (flags & ERR_PID)
        curpos = fmt_strdots(curpos, bufend,
                             "pid=%d: ", (int)getpid());
    curpos = fmt_string(curpos, bufend, format, args);
    if (flags & ERR_ERRNO)
        curpos = fmt_strdots(curpos, bufend,
                             "error (%d) %s\n", errnum, strerror(errnum));
    /* There's no sensible way to handle short writes! */
    write(fd, buffer, curpos - buffer);
}
#endif /* USE_STDERR_FILEDESC */

/* Most fundamental (and flexible) error message printing routine - always returns */
static void (err_vrf_print)(FILE *fp, int flags, const char *format, va_list args)
{
    int errnum = errno;     /* Capture errno before it is damaged! */

    if ((flags & ERR_NOFLUSH) == 0)
        fflush(0);

#if defined(USE_STDERR_SYSLOG)
    if (errlog)
        err_syslog(flags, errnum, format, args);
    else
#endif /* USE_STDERR_SYSLOG */
#if defined(USE_STDERR_FILEDESC)
    if (err_fd >= 0)
        err_filedes(err_fd, flags, errnum, format, args);
    else
#endif /* USE_STDERR_FILEDESC */
        err_stdio(fp, flags, errnum, format, args);

    fflush(fp);
}

/* Terminate program - abort or exit */
static void err_terminate(int flags, int estat)
{
    assert(flags & (ERR_ABORT|ERR_EXIT));
    if (flags & ERR_ABORT)
        abort();
    exit(estat);
}

/* Most fundamental (and flexible) error message printing routine - may return */
static void (err_vcf_print)(FILE *fp, int flags, int estat, const char *format, va_list args)
{
    err_vrf_print(fp, flags, format, args);
    if (flags & (ERR_ABORT|ERR_EXIT))
        err_terminate(flags, estat);
}

/* Analog of err_vcf_print() which guarantees 'no return' */
static void (err_vxf_print)(FILE *fp, int flags, int estat, const char *format, va_list args)
{
    err_vrf_print(fp, flags, format, args);
    err_terminate(flags, estat);
}

/* External interface to err_vcf_print() - may return */
void (err_logmsg)(FILE *fp, int flags, int estat, const char *format, ...)
{
    va_list         args;

    va_start(args, format);
    err_vcf_print(fp, flags, estat, format, args);
    va_end(args);
}

/* Print error message to current error output - no return */
static void (err_vxn_print)(int flags, int estat, const char *format, va_list args)
{
    if (errout == 0)
        errout = stderr;
    err_vxf_print(errout, flags, estat, format, args);
}

/* Print error message to current error output - no return */
static void err_exn_print(int flags, int estat, const char *format, ...)
{
    va_list args;

    va_start(args, format);
    err_vxn_print(flags, estat, format, args);
    va_end(args);
}

/* Print error message to nominated output - always returns */
static void err_erf_print(FILE *fp, int flags, const char *format, ...)
{
    va_list args;

    va_start(args, format);
    err_vrf_print(fp, flags, format, args);
    va_end(args);
}

/* Print message using current error file */
void (err_print)(int flags, int estat, const char *format, va_list args)
{
    if (errout == 0)
        errout = stderr;
    err_vcf_print(errout, flags, estat, format, args);
}

static void err_vrn_print(int flags, const char *format, va_list args)
{
    if (errout == 0)
        errout = stderr;
    err_vrf_print(errout, flags, format, args);
}

/* Report warning including message from errno */
void (err_sysrem)(const char *format, ...)
{
    va_list         args;

    va_start(args, format);
    err_vrn_print(ERR_SYSREM | err_getlogopts(), format, args);
    va_end(args);
}

/* Report error including message from errno */
void (err_syserr)(const char *format, ...)
{
    va_list         args;

    va_start(args, format);
    err_vxn_print(ERR_SYSERR | err_getlogopts(), ERR_STAT, format, args);
    va_end(args);
}

/* Report warning */
void (err_remark)(const char *format, ...)
{
    va_list         args;

    va_start(args, format);
    err_vrn_print(ERR_REM | err_getlogopts(), format, args);
    va_end(args);
}

/* Report error */
void (err_error)(const char *format, ...)
{
    va_list         args;

    va_start(args, format);
    err_vxn_print(ERR_ERR | err_getlogopts(), ERR_STAT, format, args);
    va_end(args);
}

/* Report message - sometimes exiting too */
void (err_report)(int flags, int estat, const char *format, ...)
{
    va_list         args;

    va_start(args, format);
    err_print(flags, estat, format, args);
    va_end(args);
}

/* Print usage message and exit with failure status */
void (err_usage)(const char *s1)
{
    err_exn_print(ERR_NOARG0|ERR_EXIT, EXIT_FAILURE, "Usage: %s %s\n", err_getarg0(), s1);
}

/* Report failure and generate core dump */
void (err_abort)(const char *format, ...)
{
    va_list         args;

    va_start(args, format);
    err_vxn_print(ERR_ABORT | err_getlogopts(), EXIT_FAILURE, format, args);
    va_end(args);
}

/* Report version information (no exit), removing embedded RCS keyword strings (but not values) */
void (err_printversion)(const char *program, const char *verinfo)
{
    char buffer[64];

    if (strchr(verinfo, '$'))
        verinfo = err_rcs_string(verinfo, buffer, sizeof(buffer));
    err_erf_print(stdout, ERR_DEFAULT, "%s Version %s\n", program, verinfo);
}

/* Report version information and exit, removing embedded RCS keyword strings (but not values) */
void (err_version)(const char *program, const char *verinfo)
{
    err_printversion(program, verinfo);
    exit(EXIT_SUCCESS);
}

/* Report an internal error and exit */
/* Abort if JLSS_INTERNAL_ERROR_ABORT set in environment */
void (err_internal)(const char *function, const char *format, ...)
{
    va_list args;
    int flags = ERR_EXIT;
    const char *ev = getenv("JLSS_INTERNAL_ERROR_ABORT");

    va_start(args, format);
    if (ev != 0 && *ev != '\0')
        flags = ERR_ABORT;  /* Generate core dump */
    err_remark("unrecoverable internal error in function %s():\n", function);
    err_vxn_print(flags | err_getlogopts(), EXIT_FAILURE, format, args);
    va_end(args);
}

#ifdef TEST

#error Use separate test program test.stderr.c

#endif /* TEST */
