/*
@(#)File:           $RCSfile: errhelp.c,v $
@(#)Version:        $Revision: 8.5 $
@(#)Last changed:   $Date: 2009/03/02 19:13:51 $
@(#)Purpose:        Print usage and help message in standard format.
@(#)Author:         J Leffler
@(#)Copyright:      (C) JLSS 2007-09
@(#)Product:        TIMEOUT Version 4.11 (2010-12-04)
*/

/*TABSTOP=4*/

#include "stderr.h"
#include <stdlib.h>

#ifndef lint
/* Prevent over-aggressive optimizers from eliminating ID string */
extern const char jlss_id_errhelp_c[];
const char jlss_id_errhelp_c[] = "@(#)$Id: errhelp.c,v 8.5 2009/03/02 19:13:51 jleffler Exp $";
#endif /* lint */

void err_helplist(const char *use_str, const char * const *hlp_lst)
{
    FILE *fp = stdout;
    err_logmsg(fp, ERR_REM|ERR_NOARG0, EXIT_SUCCESS,
               "Usage: %s %s\n%s\n", err_getarg0(), use_str, hlp_lst[0]);
    while (*++hlp_lst != 0)
        fprintf(fp, "%s", hlp_lst[0]);
    exit(EXIT_SUCCESS);
}

void err_help(const char *use_str, const char *hlp_str)
{
    err_logmsg(stdout, ERR_ERR|ERR_NOARG0, EXIT_SUCCESS,
               "Usage: %s %s\n%s\n", err_getarg0(), use_str, hlp_str);
    /*NOTREACHED*/
    exit(EXIT_FAILURE);
}

#ifdef TEST

#include <unistd.h>

static const char usestr[] = "[-hTV] [file ...]";

static const char hlpstr[] =
    "-V    Print version information and exit\n"
    "-T    Test operation\n"
    "-h    Print this help message\n"
    ;

static const char *hlplst[] =
{
    "\nUse a help list when the help string is too long.\n"
    "The first help list item is normally a summary of what goes on.\n"
    "It is often beneficial to start it with a newline, as demonstrated here.\n"
    "The last line of the first item does not need a newline - but if present\n"
    "it separates the summary from the subsequent output.\n",

    "-V    Print (list variant) version information and exit\n",

    "-T    Test operation\n"
    "-h    Print (list variant) this help message\n",

    0
};

int main(int argc, char **argv)
{
    err_setarg0(argv[0]);
    if (fork() == 0)
        err_help(usestr, hlpstr);
    else
    {
        sleep(1);
        err_helplist(usestr, hlplst);
    }
    return(0);
}

#endif /* TEST */
