from inspect import *
import linecache

def findsource(object):
    """Return the entire source file and starting line number for an object.

    The argument may be a module, class, method, function, traceback, frame,
    or code object.  The source code is returned as a list of all the lines
    in the file and the line number indexes a line in that list.  An IOError
    is raised if the source code cannot be retrieved."""
    file = getsourcefile(object) or getfile(object)
    module = getmodule(object, file)
    file = getsourcefile(module) or getfile(file)
    if module:
        lines = linecache.getlines(file, module.__dict__)
    else:
        lines = linecache.getlines(file)
    if not lines:
        raise IOError('could not get source code')

    if ismodule(object):
        return lines, 0

    if isclass(object):
        name = object.__name__
        pat = re.compile(r'^(\s*)class\s*' + name + r'\b')
        # make some effort to find the best matching class definition:
        # use the one with the least indentation, which is the one
        # that's most probably not inside a function definition.
        candidates = []
        for i in range(len(lines)):
            match = pat.match(lines[i])
            if match:
                # if it's at toplevel, it's already the best one
                if lines[i][0] == 'c':
                    return lines, i
                # else add whitespace to candidate list
                candidates.append((match.group(1), i))
        if candidates:
            # this will sort by whitespace, and by line number,
            # less whitespace first
            candidates.sort()
            return lines, candidates[0][1]
        else:
            raise IOError('could not find class definition')

    if ismethod(object):
        object = object.__func__
    if isfunction(object):
        object = object.__code__
    if istraceback(object):
        object = object.tb_frame
    if isframe(object):
        object = object.f_code
    if iscode(object):
        if not hasattr(object, 'co_firstlineno'):
            raise IOError('could not find function definition')
        lnum = object.co_firstlineno - 1
        pat = re.compile(r'^(\s*def\s)|(.*(?<!\w)lambda(:|\s))|^(\s*@)')
        while lnum > 0:
            if pat.match(lines[lnum]): break
            lnum = lnum - 1
        return lines, lnum
    raise IOError('could not find code object')

def getsourcelines(object):
    """Return a list of source lines and starting line number for an object.

    The argument may be a module, class, method, function, traceback, frame,
    or code object.  The source code is returned as a list of the lines
    corresponding to the object and the line number indicates where in the
    original source file the first line of code was found.  An IOError is
    raised if the source code cannot be retrieved."""
    lines, lnum = findsource(object)

    if ismodule(object): return lines, 0
    else: return getblock(lines[lnum:]), lnum + 1

def getsource(object):
    """Return the text of the source code for an object.

    The argument may be a module, class, method, function, traceback, frame,
    or code object.  The source code is returned as a single string.  An
    IOError is raised if the source code cannot be retrieved."""
    lines, lnum = getsourcelines(object)
    return ''.join(lines)
