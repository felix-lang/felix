#line 635 "../lpsrc/flx_glob.pak"
#ifndef __FLX_GLOB_H__
#define __FLX_GLOB_H__
#include <flx_glob_config.hpp>

#if !FLX_WIN32
#include <glob.h>
#else

/* Error codes */
#define GLOB_NOSPACE    (1)             /*!< \brief (Error result code:) An attempt to allocate memory failed, or if errno was 0 GLOB_LIMIT was specified in the flags and ARG_MAX patterns were matched. */
#define GLOB_ABORTED    (2)             /*!< \brief (Error result code:) The scan was stopped because an error was encountered and either GLOB_ERR was set or (*errfunc)() returned non-zero. */
#define GLOB_NOMATCH    (3)             /*!< \brief (Error result code:) The pattern does not match any existing pathname, and GLOB_NOCHECK was not set int flags. */
#define GLOB_NOSYS      (4)             /*!< \brief (Error result code:) . */
#define GLOB_ABEND      GLOB_ABORTED    /*!< \brief (Error result code:) . */

/* Flags */
#define GLOB_ERR            0x00000001  /*!< \brief Return on read errors. */
#define GLOB_MARK           0x00000002  /*!< \brief Append a slash to each name. */
#define GLOB_NOSORT         0x00000004  /*!< \brief Don't sort the names. */
#define GLOB_DOOFFS         0x00000008  /*!< \brief Insert PGLOB->gl_offs NULLs. Supported from version 1.6 of UNIXEm. */
#define GLOB_NOCHECK        0x00000010  /*!< \brief If nothing matches, return the pattern. Supported from version 1.6 of UNIXEm. */
#define GLOB_APPEND         0x00000020  /*!< \brief Append to results of a previous call. Not currently supported in this implementation. */
#define GLOB_NOESCAPE       0x00000040  /*!< \brief Backslashes don't quote metacharacters. Has no effect in this implementation, since escaping is not supported. */

#define GLOB_PERIOD         0x00000080  /*!< \brief Leading `.' can be matched by metachars. Supported from version 1.6 of UNIXEm. */
#define GLOB_MAGCHAR        0x00000100  /*!< \brief Set in gl_flags if any metachars seen. Supported from version 1.6 of UNIXEm. */
/* #define GLOB_ALTDIRFUNC     0x00000200 */  /*!< \brief Use gl_opendir et al functions. Not currently supported in this implementation. */
/* #define GLOB_BRACE          0x00000400 */  /*!< \brief Expand "{a,b}" to "a" "b". Not currently supported in this implementation. */
#define GLOB_NOMAGIC        0x00000800  /*!< \brief If no magic chars, return the pattern. Supported from version 1.6 of UNIXEm. */
#define GLOB_TILDE          0x00001000  /*!< \brief Expand ~user and ~ to home directories. Partially supported from version 1.6 of UNIXEm: leading ~ is expanded to %HOMEDRIVE%%HOMEPATH%. */
#define GLOB_ONLYDIR        0x00002000  /*!< \brief Match only directories. This implementation guarantees to only return directories when this flag is specified. */
#define GLOB_TILDE_CHECK    0x00004000  /*!< \brief Like GLOB_TILDE but return an GLOB_NOMATCH even if GLOB_NOCHECK specified. Supported from version 1.6 of UNIXEm. */
#define GLOB_ONLYFILE       0x00008000  /*!< \brief Match only files. Supported from version 1.6 of UNIXEm. */
#define GLOB_NODOTSDIRS     0x00010000  /*!< \brief Elide "." and ".." directories from wildcard searches. Supported from version 1.6 of UNIXEm. */
#define GLOB_LIMIT          0x00020000  /*!< \brief Limits the search to the number specified by the caller in gl_matchc. Supported from version 1.6 of UNIXEm. */

typedef struct
{
  int   gl_pathc;   /*!< count of total paths so far */
  int   gl_matchc;  /*!< count of paths matching pattern */
  int   gl_offs;    /*!< reserved at beginning of gl_pathv */
  int   gl_flags;   /*!< returned flags */
  char  **gl_pathv; /*!< list of paths matching pattern */
} glob_t;

int GLOB_EXTERN glob( char const  *pattern
        , int         flags
        , int       (*errfunc)(char const *, int)
        , glob_t      *pglob);

void GLOB_EXTERN globfree(glob_t *pglob);

#endif
#endif

