import fbuild.builders.c.gcc

# ------------------------------------------------------------------------------

class Ar(fbuild.builders.c.gcc.Ar):
    def __init__(self, ctx, exe='avr-ar', ranlib='avr-ranlib', **kwargs):
        super().__init__(ctx, exe, ranlib=ranlib, **kwargs)

# ------------------------------------------------------------------------------

class Gcc(fbuild.builders.c.gcc.Gcc):
    """Overload Gcc's builder to add the avr-gcc options."""

    def __init__(self, *args, mmcu, pre_flags=[], **kwargs):
        self.mmcu = mmcu

        pre_flags = list(pre_flags)
        pre_flags.append('-mmcu=' + mmcu)

        super().__init__(*args, pre_flags=pre_flags, **kwargs)

# ------------------------------------------------------------------------------

def make_cc(ctx, exe=None, default_exes=['avr-gcc'], **kwargs):
    return Gcc(ctx,
        fbuild.builders.find_program(ctx, [exe] if exe else default_exes),
        **kwargs)

# ------------------------------------------------------------------------------

@fbuild.db.caches
def static(*args, make_cc=make_cc, **kwargs):
    return fbuild.builders.c.gcc.static(*args,
        make_cc=make_cc,
        make_lib_linker=fbuild.builders.ar.avr.Ar,
        cross_compiler=True,
        **kwargs)

@fbuild.db.caches
def shared(*args, make_cc=make_cc, **kwargs):
    return fbuild.builders.c.gcc.shared(*args,
        make_cc=make_cc,
        cross_compiler=True,
        **kwargs)
