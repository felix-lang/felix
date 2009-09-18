import difflib
import time

import fbuild
import fbuild.builders.scala
import fbuild.builders.java
import fbuild.db
from fbuild.path import Path

# ------------------------------------------------------------------------------

def run_tests(target):
    # See if java's on the system.
    try:
        java = fbuild.builders.java.Builder(target.ctx)
    except fbuild.ConfigFailed:
        # Oh well, it was a good try...
        java = None

    # See if scala's on the system.
    try:
        scala = fbuild.builders.scala.Builder(target.ctx, optimize=True)
    except fbuild.ConfigFailed:
        # Oh well, it was a good try...
        scala = None

    # Now, run all the tests we can find.
    for test in Path('speed/*').glob():
        if not test.isdir():
            continue

        expect = test / 'expect'

        headers = ['lang', 'time', 'wall clock']
        widths = [len(h) for h in headers]

        results = []
        for path in (test/ '*').glob():
            if not path.isdir():
                continue

            for result in run_language_tests(path, expect, target, java, scala):
                # adjust the widths to the maximum column size
                for i in range(len(widths)):
                    widths[i] = max(len(result[i]), widths[i])

                results.append(result)

        # now summarize the results
        target.ctx.logger.log('')
        target.ctx.logger.check('results:', test, color='green')

        def p(columns):
            s = ' | '.join(columns[i].ljust(widths[i]) for i in range(len(widths)))
            target.ctx.logger.log(s)

        p(headers)
        target.ctx.logger.log(' | '.join('-' * w for w in widths))

        for result in results:
            p(result)

        target.ctx.logger.log('')

# ------------------------------------------------------------------------------

def run_language_tests(path, expect, target, java, scala):
    dst = path / 'test'

    subtests = []
    srcs = []
    for src in (path / '*').glob():
        if src.isdir():
            subtests.append(src)
        else:
            srcs.append(src)

    if srcs:
        try:
            internal_time, wall_time = run_test(target.ctx, path, dst, srcs,
                expect=expect if expect.exists() else None,
                c=target.c.static,
                cxx=target.cxx.static,
                felix=target.felix,
                java=java,
                ocaml=target.ocaml,
                scala=scala)
        except (ValueError, fbuild.Error) as e:
            target.ctx.logger.log(e)
            internal_time = None
            wall_time = None

        # If we didn't get values back, just print N/A
        if internal_time is None:
            internal_time = 'N/A'
        else:
            internal_time = '%.4f' % float(internal_time)

        if wall_time is None:
            wall_time = 'N/A'
        else:
            wall_time = '%.4f' % float(wall_time)

        yield path, internal_time, wall_time

    for subtest in subtests:
        for result in run_language_tests(subtest, expect, target, java, scala):
            yield result

# ------------------------------------------------------------------------------

@fbuild.db.caches
def run_test(ctx, path, dst, srcs:fbuild.db.SRCS, expect:fbuild.db.OPTIONAL_SRC,
        *,
        c,
        cxx,
        felix,
        java,
        ocaml,
        scala):
    lang = path.name.rsplit(':')[0]

    # Compile the executable
    try:
        if lang == 'c':
            exe = c.build_exe(dst, srcs)
        elif lang == 'c++':
            exe = cxx.build_exe(dst, srcs)
        elif lang == 'felix':
            # All the felix tests are named test.flx
            exe = felix.compile(path / 'test.flx', static=True)
        elif lang == 'java':
            # Exit early if java isn't supported
            if java is None:
                return None, None
            exe = java.build_lib(dst + '.jar', srcs, cwd=dst.parent)
        elif lang == 'ocaml':
            exe = ocaml.build_exe(dst, srcs, external_libs=['unix'])
        elif lang == 'scala':
            # Exit early if scala isn't supported
            if scala is None:
                return None, None
            exe = scala.build_lib(dst + '.jar', srcs, cwd=dst.parent)
        else:
            ctx.logger.check('do not know how to build', path,
                color='yellow')
            return None, None
    except fbuild.ExecutionError:
        # If we failed to compile, just move on
        return None, None

    fbuild.db.add_external_dependencies_to_call(ctx, dsts=[exe])

    # Run the executable and measure the wall clock time
    ctx.logger.check('running ' + exe)
    t0 = time.time()
    try:
        if lang == 'java':
            stdout, stderr = java.run_class('Test',
                classpaths=[exe.name],
                cwd=exe.parent,
                timeout=60,
                quieter=1)
        elif lang == 'scala':
            # We have to be careful with scala since it doesn't
            # like ':'s in it's classpaths.
            stdout, stderr = scala.run_class('Test',
                classpaths=[exe.name],
                cwd=exe.parent,
                timeout=60,
                quieter=1)
        else:
            stdout, stderr = ctx.execute(exe, timeout=60, quieter=1)
    except fbuild.ExecutionTimedOut as e:
        t1 = time.time()
        ctx.logger.failed('failed: timed out')

        ctx.logger.log(e, verbose=1)
        if e.stdout:
            ctx.logger.log(e.stdout.decode().strip(), verbose=1)
        if e.stderr:
            ctx.logger.log(e.stderr.decode().strip(), verbose=1)

        internal_time = None
        wall_time = time.time() - t0
    else:
        wall_time = time.time() - t0

        if expect is None:
            internal_time = float(stdout.decode())

            ctx.logger.passed('ok: %.4f time %.4f total' %
                (internal_time, wall_time))
        else:
            stdout = stdout.replace(b'\r\n', b'\n').replace(b'\r', b'\n')
            stdout, internal_time, _ = stdout.rsplit(b'\n', 2)
            stdout = stdout + b'\n'

            with open(expect, 'rb') as f:
                s = f.read().replace(b'\r\n', b'\n').replace(b'\r', b'\n')

            if stdout == s:
                internal_time = float(internal_time.decode())

                ctx.logger.passed('ok: %.4f time %.4f total' %
                    (internal_time, wall_time))
            else:
                internal_time = None

                ctx.logger.failed('failed: output does not match')

                for line in difflib.ndiff(
                        stdout.decode().split('\n'),
                        s.decode().split('\n')):
                    ctx.logger.log(line)

    return internal_time, wall_time
