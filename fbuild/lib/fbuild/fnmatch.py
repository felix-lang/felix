"""Filename matching with shell patterns.

fnmatch(FILENAME, PATTERN) matches according to the local convention.
fnmatchcase(FILENAME, PATTERN) always takes case in account.

The functions operate by translating the pattern into a regular
expression.  They cache the compiled regular expressions for speed.

The function translate(PATTERN) returns a regular expression
corresponding to PATTERN.  (It does not compile it.)
"""

import re

__all__ = ["filter", "fnmatch","fnmatchcase","translate"]

_cache = {}  # Maps text patterns to compiled regexen.
_cacheb = {}  # Ditto for bytes patterns.

def fnmatch(name, pat):
    """Test whether FILENAME matches PATTERN.

    Patterns are Unix shell style:

    *           matches everything
    ?           matches any single character
    [seq]       matches any character in seq
    [!seq]      matches any char not in seq
    {pat1,pat2} matches subpattern pat1 or subpattern pat2

    An initial period in FILENAME is not special.
    Both FILENAME and PATTERN are first case-normalized
    if the operating system requires it.
    If you don't want this, use fnmatchcase(FILENAME, PATTERN).
    """

    import os
    name = os.path.normcase(name)
    pat = os.path.normcase(pat)
    return fnmatchcase(name, pat)

def _compile_pattern(pat):
    cache = _cacheb if isinstance(pat, bytes) else _cache
    regex = cache.get(pat)
    if regex is None:
        if isinstance(pat, bytes):
            pat_str = str(pat, 'ISO-8859-1')
            res_str = translate(pat_str)
            res = bytes(res_str, 'ISO-8859-1')
        else:
            res = translate(pat)
        cache[pat] = regex = re.compile(res)
    return regex.match

def filter(names, pat):
    """Return the subset of the list NAMES that match PAT"""
    import os,posixpath
    result = []
    pat = os.path.normcase(pat)
    match = _compile_pattern(pat)
    if os.path is posixpath:
        # normcase on posix is NOP. Optimize it away from the loop.
        for name in names:
            if match(name):
                result.append(name)
    else:
        for name in names:
            if match(os.path.normcase(name)):
                result.append(name)
    return result

def fnmatchcase(name, pat):
    """Test whether FILENAME matches PATTERN, including case.

    This is a version of fnmatch() which doesn't case-normalize
    its arguments.
    """

    match = _compile_pattern(pat)
    return match(name) is not None

def translate(pat):
    """Translate a shell PATTERN to a regular expression.

    There is no way to quote meta-characters.
    """

    return _translate(0, pat, '')[2] + '$'

def _translate(i, pat, end):
    res = ''
    n = len(pat)
    while i < n:
        c = pat[i]
        if c in end:
            return i, c, res
        i = i+1
        if c == '*':
            res = res + '.*'
        elif c == '?':
            res = res + '.'
        elif c == '[':
            j = i
            if j < n and pat[j] == '!':
                j = j+1
            if j < n and pat[j] == ']':
                j = j+1
            while j < n and pat[j] != ']':
                j = j+1
            if j >= n:
                res = res + '\\['
            else:
                stuff = pat[i:j].replace('\\','\\\\')
                i = j+1
                if stuff[0] == '!':
                    stuff = '^' + stuff[1:]
                elif stuff[0] == '^':
                    stuff = '\\' + stuff
                res = '%s[%s]' % (res, stuff)
        elif c == '{':
            i, sub = _translate_subexpression(i, pat)
            res += sub
        else:
            res = res + re.escape(c)
    return i, '', res

def _translate_subexpression(i, pat):
    j = i
    subexpressions = []
    while True:
        j, c, res = _translate(j, pat, ',}')
        subexpressions.append(res)

        if c == ',':
            j += 1
        elif c == '}':
            j += 1
            break
        else:
            # turns out we didn't have a subpattern
            return j, '{' + ','.join(subexpressions)

    return j, '(' + '|'.join(subexpressions) + ')'
