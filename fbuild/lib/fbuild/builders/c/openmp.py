import fbuild.db
from fbuild.builders.c import MissingHeader
from fbuild.record import Record

# -----------------------------------------------------------------------------

@fbuild.db.caches
def config_omp_h(ctx, builder):
    if not builder.check_header_exists('omp.h'):
        raise MissingHeader('omp.h')

    code = r'''
        #include <omp.h>
        #include <stdio.h>
        #include <stdlib.h>

        int main (int argc, char *argv[]) {
            int nthreads, tid;

            #pragma omp parallel private(nthreads, tid)
            {
                tid = omp_get_thread_num();
                printf("Hello World from thread = %d\n", tid);

                /* Only master thread does this */
                if (tid == 0) {
                    nthreads = omp_get_num_threads();
                    printf("Number of threads = %d\n", nthreads);
                }
            }

            return 0;
        }
    '''

    ctx.logger.check('checking if supports omp_get_thread_num')
    for flags in [], ['-openmp'], ['-fopenmp'], ['/openmp']:
        if builder.try_run(code, lkwargs={'flags': flags}):
            ctx.logger.passed('ok %r' % ' '.join(flags))

            return Record(flags=flags)
    else:
        ctx.logger.failed()
        raise fbuild.ConfigFailed('failed to link openmp program')


def config(ctx, builder):
    return Record(
        headers=Record(
            omp_h=config_omp_h(ctx, builder),
        )
    )
