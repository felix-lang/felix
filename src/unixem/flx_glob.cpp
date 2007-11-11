#line 692 "../lpsrc/flx_glob.pak"
#include "flx_glob.hpp"
#include "unixem_util.hpp"

#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <windows.h>

static char const *strrpbrk(char const *string, char const *strCharSet)
{
    char const *part   =   NULL;
    char const  *pch;

    for(pch = strCharSet; *pch; ++pch)
    {
        char const *p  =   strrchr(string, *pch);

        if(NULL != p)
        {
            if(NULL == part)
            {
                part = p;
            }
            else
            {
                if(part < p)
                {
                    part = p;
                }
            }
        }
    }

    return part;
}

int glob(   char const  *pattern
        ,   int         flags
        , int         (*errfunc)(char const *, int)
        ,   glob_t      *pglob)
{
    int                 result;
    char                szRelative[1 + _MAX_PATH];
    char const          *file_part;
    WIN32_FIND_DATAA    find_data;
    HANDLE              hFind;
    char                *buffer;
    char                szPattern2[1 + _MAX_PATH];
    char                szPattern3[1 + _MAX_PATH];
    char const          *effectivePattern   =   pattern;
    char const          *leafMost;
    const int           bMagic              =   (NULL != strpbrk(pattern, "?*"));
    int                 bNoMagic            =   0;
    int                 bMagic0;
    size_t              maxMatches          =   ~(size_t)(0);

    if(flags & GLOB_NOMAGIC)
    {
        bNoMagic = !bMagic;
    }

    if(flags & GLOB_LIMIT)
    {
        maxMatches = (size_t)pglob->gl_matchc;
    }

    if(flags & GLOB_TILDE)
    {
        if( '~' == pattern[0] &&
            (   '\0' == pattern[1] ||
                '/' == pattern[1] ||
                '\\' == pattern[1]))
        {
            DWORD   dw;

            (void)lstrcpyA(&szPattern2[0], "%HOMEDRIVE%%HOMEPATH%");

            dw = ExpandEnvironmentStringsA(&szPattern2[0], &szPattern3[0], UNIXEM_NUM_ELEMENTS(szPattern3) - 1);

            if(0 != dw)
            {
                (void)lstrcpynA(&szPattern3[0] + dw - 1, &pattern[1], (int)(UNIXEM_NUM_ELEMENTS(szPattern3) - dw));
                szPattern3[UNIXEM_NUM_ELEMENTS(szPattern3) - 1] = '\0';

                effectivePattern = szPattern3;
            }
        }
    }

    file_part = strrpbrk(effectivePattern, "\\/");

    if(NULL != file_part)
    {
        leafMost = ++file_part;

        (void)lstrcpyA(szRelative, effectivePattern);
        szRelative[file_part - effectivePattern] = '\0';
    }
    else
    {
        szRelative[0] = '\0';
        leafMost = effectivePattern;
    }

    bMagic0 =   (leafMost == strpbrk(leafMost, "?*"));

    hFind   =   FindFirstFileA(effectivePattern, &find_data);
    buffer  =   NULL;

    pglob->gl_pathc = 0;
    pglob->gl_pathv = NULL;

    if(0 == (flags & GLOB_DOOFFS))
    {
        pglob->gl_offs = 0;
    }

    if(hFind == INVALID_HANDLE_VALUE)
    {
        if(NULL != errfunc)
        {
            (void)errfunc(effectivePattern, (int)GetLastError());
        }

        result = GLOB_NOMATCH;
    }
    else
    {
        int     cbCurr      =   0;
        size_t  cbAlloc     =   0;
        size_t  cMatches    =   0;

        result = 0;

        do
        {
            int     cch;
            size_t  new_cbAlloc;

            if( bMagic0 &&
                0 == (flags & GLOB_PERIOD))
            {
                if('.' == find_data.cFileName[0])
                {
                    continue;
                }
            }

            if(find_data.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
            {
#ifdef GLOB_ONLYFILE
                if(flags & GLOB_ONLYFILE)
                {
                    continue;
                }
#endif /* GLOB_ONLYFILE */

                if( bMagic0 &&
                    GLOB_NODOTSDIRS == (flags & GLOB_NODOTSDIRS))
                {
                    /* Pattern must begin with '.' to match either dots directory */
                    if( 0 == lstrcmpA(".", find_data.cFileName) ||
                        0 == lstrcmpA("..", find_data.cFileName))
                    {
                        continue;
                    }
                }

                if(flags & GLOB_MARK)
                {
#if 0
                    if(find_data.cFileName[0] >= 'A' && find_data.cFileName[0] <= 'M')
#endif /* 0 */
                    (void)lstrcatA(find_data.cFileName, "/");
                }
            }
            else
            {
                if(flags & GLOB_ONLYDIR)
                {
                    /* Skip all further actions, and get the next entry */
#if 0
                    if(find_data.cFileName[0] >= 'A' && find_data.cFileName[0] <= 'M')
#endif /* 0 */
                    continue;
                }
            }

            cch =   lstrlenA(find_data.cFileName);
            if(NULL != file_part)
            {
                cch +=  file_part - effectivePattern;
            }

            new_cbAlloc = (size_t)cbCurr + cch + 1;
            if(new_cbAlloc > cbAlloc)
            {
                char    *new_buffer;

                new_cbAlloc *= 2;

                new_cbAlloc = (new_cbAlloc + 31) & ~(31);

                new_buffer  = (char*)realloc(buffer, new_cbAlloc);

                if(new_buffer == NULL)
                {
                    result = GLOB_NOSPACE;
                    free(buffer);
                    buffer = NULL;
                    break;
                }

                buffer = new_buffer;
                cbAlloc = new_cbAlloc;
            }

            (void)lstrcpynA(buffer + cbCurr, szRelative, 1 + (file_part - effectivePattern));
            (void)lstrcatA(buffer + cbCurr, find_data.cFileName);
            cbCurr += cch + 1;

            ++cMatches;
        }
        while(FindNextFile(hFind, &find_data) && cMatches != maxMatches);

        (void)FindClose(hFind);

        if(result == 0)
        {
            /* Now expand the buffer, to fit in all the pointers. */
            size_t  cbPointers  =   (1 + cMatches + pglob->gl_offs) * sizeof(char*);
            char    *new_buffer =   (char*)realloc(buffer, cbAlloc + cbPointers);

            if(new_buffer == NULL)
            {
                result = GLOB_NOSPACE;
                free(buffer);
            }
            else
            {
                char    **pp;
                char    **begin;
                char    **end;
                char    *next_str;

                buffer = new_buffer;

                (void)memmove(new_buffer + cbPointers, new_buffer, cbAlloc);

                /* Handle the offsets. */
                begin =   (char**)new_buffer;
                end   =   begin + pglob->gl_offs;

                for(; begin != end; ++begin)
                {
                    *begin = NULL;
                }

                /* Sort, or no sort. */
                pp    =   (char**)new_buffer + pglob->gl_offs;
                begin =   pp;
                end   =   begin + cMatches;

                if(flags & GLOB_NOSORT)
                {
                    /* The way we need in order to test the removal of dots in the findfile_sequence. */
                    *end = NULL;
                    for(begin = pp, next_str = buffer + cbPointers; begin != end; --end)
                    {
                        *(end - 1) = next_str;

                        /* Find the next string. */
                        next_str += 1 + lstrlenA(next_str);
                    }
                }
                else
                {
                    /* The normal way. */
                    for(begin = pp, next_str = buffer + cbPointers; begin != end; ++begin)
                    {
                        *begin = next_str;

                        /* Find the next string. */
                        next_str += 1 + lstrlenA(next_str);
                    }
                    *begin = NULL;
                }

                /* Return results to caller. */
                pglob->gl_pathc =   (int)cMatches;
                pglob->gl_matchc=   (int)cMatches;
                pglob->gl_flags =   0;
                if(bMagic)
                {
                    pglob->gl_flags |= GLOB_MAGCHAR;
                }
                pglob->gl_pathv =   (char**)new_buffer;
            }
        }

        if(0 == cMatches)
        {
            result = GLOB_NOMATCH;
        }
    }

    if(GLOB_NOMATCH == result)
    {
        if( (flags & GLOB_TILDE_CHECK) &&
            effectivePattern == szPattern3)
        {
            result = GLOB_NOMATCH;
        }
        else if(bNoMagic ||
                (flags & GLOB_NOCHECK))
        {
            size_t  cbNeeded    =   ((2 + pglob->gl_offs) * sizeof(char*)) + (1 + strlen(effectivePattern));
            char    **pp        =   (char**)realloc(buffer, cbNeeded);

            if(NULL == pp)
            {
                result = GLOB_NOSPACE;
                free(buffer);
            }
            else
            {
                /* Handle the offsets. */
                char    **begin =   pp;
                char    **end   =   pp + pglob->gl_offs;

                for(; begin != end; ++begin)
                {
                    *begin = NULL;
                }

                /* Synthesis the pattern result. */
                pp[0 + pglob->gl_offs]  =   strcpy((char*)(pp + 2 + pglob->gl_offs), effectivePattern);
                pp[1 + pglob->gl_offs]  =   NULL;

                /* Return results to caller. */
                pglob->gl_pathc =   1;
                pglob->gl_matchc=   1;
                pglob->gl_flags =   0;
                if(bMagic)
                {
                    pglob->gl_flags |= GLOB_MAGCHAR;
                }
                pglob->gl_pathv =   pp;

                result = 0;
            }
        }
    }
    else if(0 == result)
    {
        if((size_t)pglob->gl_matchc == maxMatches)
        {
            result = GLOB_NOSPACE;
        }
    }

    return result;
}

void globfree(glob_t *pglob)
{
    if(pglob != NULL)
    {
        free(pglob->gl_pathv);
        pglob->gl_pathc = 0;
        pglob->gl_pathv = NULL;
    }
}

