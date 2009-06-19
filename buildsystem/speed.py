import difflib
import time

import fbuild
import fbuild.builders.scala
import fbuild.builders.java
import fbuild.db
from fbuild.path import Path

# ------------------------------------------------------------------------------

def run_tests(target, felix):
    # See if java's on the system.
    try:
        java = fbuild.builders.java.Builder()
    except fbuild.ConfigFailed:
        # Oh well, it was a good try...
        java = None

    # See if scala's on the system.
    try:
        scala = fbuild.builders.scala.Builder(optimize=True)
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

            dst = fbuild.buildroot / path / 'test'
            srcs = (path / '*').glob()

            try:
                internal_time, wall_time = run_test(path, dst, srcs,
                    expect=expect if expect.exists() else None,
                    c=target.c.static,
                    cxx=target.cxx.static,
                    felix=felix,
                    java=java,
                    ocaml=target.ocaml,
                    scala=scala)
            except (ValueError, fbuild.Error) as e:
                fbuild.logger.log(e)
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

            result = (path.name, internal_time, wall_time)

            # adjust the widths to the maximum column size

            for i in range(len(widths)):
                widths[i] = max(len(result[i]), widths[i])

            results.append(result)

        # now summarize the results
        fbuild.logger.log('')
        fbuild.logger.check('results:', test, color='green')

        def p(columns):
            s = ' | '.join(columns[i].ljust(widths[i]) for i in range(len(widths)))
            fbuild.logger.log(s)

        p(headers)
        fbuild.logger.log(' | '.join('-' * w for w in widths))

        for result in results:
            p(result)

        fbuild.logger.log('')

# ------------------------------------------------------------------------------

@fbuild.db.caches
def run_test(path, dst, srcs:fbuild.db.SRCS, expect:fbuild.db.OPTIONAL_SRC,
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
            fbuild.logger.check('do not know how to build', path,
                color='yellow')
            return None, None
    except fbuild.ExecutionError:
        # If we failed to compile, just move on
        return None, None

    fbuild.db.add_external_dependencies_to_call(dsts=[exe])

    # Run the executable and measure the wall clock time
    fbuild.logger.check('running ' + exe)
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
            stdout, stderr = fbuild.execute(exe, timeout=60, quieter=1)
    except fbuild.ExecutionTimedOut as e:
        t1 = time.time()
        fbuild.logger.failed('failed: timed out')

        fbuild.logger.log(e, verbose=1)
        if e.stdout:
            fbuild.logger.log(e.stdout.decode().strip(), verbose=1)
        if e.stderr:
            fbuild.logger.log(e.stderr.decode().strip(), verbose=1)

        internal_time = None
        wall_time = time.time() - t0
    else:
        wall_time = time.time() - t0

        if expect is None:
            internal_time = float(stdout.decode())

            fbuild.logger.passed('ok: %.4f time %.4f total' %
                (internal_time, wall_time))
        else:
            stdout = stdout.replace(b'\r\n', b'\n').replace(b'\r', b'\n')
            stdout, internal_time, _ = stdout.rsplit(b'\n', 2)
            stdout = stdout + b'\n'

            with open(expect, 'rb') as f:
                s = f.read().replace(b'\r\n', b'\n').replace(b'\r', b'\n')

            if stdout == s:
                internal_time = float(internal_time.decode())

                fbuild.logger.passed('ok: %.4f time %.4f total' %
                    (internal_time, wall_time))
            else:
                internal_time = None

                fbuild.logger.failed('failed: output does not match')

                for line in difflib.ndiff(
                        stdout.decode().split('\n'),
                        s.decode().split('\n')):
                    fbuild.logger.log(line)

    return internal_time, wall_time
