import fbuild.builders.cxx

def build(ctx):
    static = fbuild.builders.cxx.guess_static(ctx, platform_options=[
        ({'windows'}, {'flags': ['/EHsc']}),
    ])
    lib = static.build_lib('lib_static', ['lib.cpp'], macros=['STATIC_LINK'])
    exe = static.build_exe('exe_static', ['exe.cpp'], macros=['STATIC_LINK'],
        libs=[lib])

    ctx.logger.log(' * running %s:' % exe)
    static.run([exe])

    shared = fbuild.builders.cxx.guess_shared(ctx, platform_options=[
        ({'windows'}, {'flags': ['/EHsc']}),
    ])
    lib = shared.build_lib('lib_shared', ['lib.cpp'], macros=['BUILD_LIB'])
    exe = shared.build_exe('exe_shared', ['exe.cpp'], libs=[lib])

    ctx.logger.log(' * running %s:' % exe)
    shared.run([exe])
