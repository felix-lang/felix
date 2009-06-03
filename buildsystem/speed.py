import difflib
import time

import fbuild
import fbuild.db
from fbuild.path import Path

# ------------------------------------------------------------------------------

def run_tests(target, felix):
    for test in Path('speed/*').glob():
        if not test.isdir():
            continue

        expect = test / 'expect'

        headers = ['lang', 'time', 'wall clock']
        widths = [len(h) for h in headers]

        results = []
        for lang in (test/ '*').glob():
            if not lang.isdir():
                continue

            dst = fbuild.buildroot / lang / 'test'
            srcs = (lang / '*').glob()

            internal_time, wall_time = run_test(lang, dst, srcs,
                expect=expect if expect.exists() else None,
                c=target.c.static,
                cxx=target.cxx.static,
                felix=felix,
                ocaml=target.ocaml)

            # If we didn't get values back, just print N/A
            if internal_time is None:
                internal_time = 'N/A'
            else:
                internal_time = '%.4f' % float(internal_time)

            if wall_time is None:
                wall_time = 'N/A'
            else:
                wall_time = '%.4f' % float(wall_time)

            result = (lang.name, internal_time, wall_time)

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
def run_test(lang,
        dst:fbuild.db.DST,
        srcs:fbuild.db.SRCS,
        expect:fbuild.db.OPTIONAL_SRC,
        *,
        c,
        cxx,
        felix,
        ocaml):
    # Compile the executable
    try:
        if lang.name.rsplit(':')[0] == 'c':
            exe = c.build_exe(dst, srcs)
        elif lang.name.rsplit(':')[0] == 'c++':
            exe = cxx.build_exe(dst, srcs)
        elif lang.name.rsplit(':')[0] == 'felix':
            # All the felix tests are named test.flx
            exe = felix.compile(lang / 'test.flx', includes=[lang], static=True)
        elif lang.name.rsplit(':')[0] == 'ocaml':
            exe = ocaml.build_exe(dst, srcs, external_libs=['unix'])
        else:
            fbuild.logger.check('do not know how to build', lang.name,
                color='yellow')
            return None, None
    except fbuild.ExecutionError:
        # If we failed to compile, just move on
        return None, None

    # Run the executable and measure the wall clock time
    fbuild.logger.check('running ' + exe)
    t0 = time.time()
    try:
        stdout, stderr = fbuild.execute(exe, timeout=60, quieter=1)
    except fbuild.ExecutionError as e:
        t1 = time.time()

        if isinstance(e, fbuild.ExecutionTimedOut):
            fbuild.logger.failed('failed: timed out')
        else:
            fbuild.logger.failed()

        fbuild.logger.log(e, verbose=1)
        if e.stdout:
            fbuild.logger.log(e.stdout.decode().strip(), verbose=1)
        if e.stderr:
            fbuild.logger.log(e.stderr.decode().strip(), verbose=1)

        internal_time = None
        wall_time = None
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
