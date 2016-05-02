import fbuild.builders.c.gcc.darwin as darwin
import fbuild.builders.c.gcc.iphone as iphone

# ------------------------------------------------------------------------------

def static(ctx, exe=None, *args, **kwargs):
    if exe is None:
        exe = iphone._iphone_devroot(False) / 'usr/bin/g++'

    return iphone._builder(darwin.static, ctx, exe, *args,
        simulator=False,
        cross_compiler=True,
        **kwargs)

def shared(ctx, exe=None, *args, **kwargs):
    if exe is None:
        exe = iphone._iphone_devroot(False) / 'usr/bin/g++'

    return iphone._builder(darwin.shared, ctx, exe, *args,
        simulator=False,
        cross_compiler=True,
        **kwargs)

# ------------------------------------------------------------------------------

def static_simulator(ctx, exe=None, *args, **kwargs):
    if exe is None:
        exe = iphone._iphone_devroot(True) / 'usr/bin/g++'

    return iphone._builder(darwin.static, ctx, exe, *args,
        simulator=True,
        **kwargs)

def shared_simulator(ctx, exe=None, *args, **kwargs):
    if exe is None:
        exe = iphone._iphone_devroot(True) / 'usr/bin/g++'

    return iphone._builder(darwin.shared, ctx, exe, *args,
        simulator=True,
        **kwargs)
