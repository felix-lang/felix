import fbuild.builders.felix
includes = ['/usr/include','.']
executables = ['webapp.flx']
def build(ctx):
    builder = fbuild.builders.felix.Felix(ctx)
    for src in executables:
        exe = builder.compile(src,includes=includes,static=True)
    ctx.logger.log(' * running ' + exe)
    ctx.execute([exe])


