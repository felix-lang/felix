import fbuild.builders.c

# ------------------------------------------------------------------------------

def guess_static(*args, **kwargs):
    """L{static} tries to guess the static system c++ compiler according to the
    platform. It accepts a I{platform} keyword that overrides the system's
    platform. This can be used to use a non-default compiler. Any extra
    arguments and keywords are passed to the compiler's configuration
    functions."""

    return fbuild.builders.c._guess_builder('c++ static', {'g++', 'clang++'}, (
        ({'iphone', 'simulator', 'g++'},
            'fbuild.builders.cxx.gxx.iphone.static_simulator'),
        ({'iphone', 'g++'}, 'fbuild.builders.cxx.gxx.iphone.static'),
        ({'darwin', 'clang++'}, 'fbuild.builders.cxx.clangxx.darwin.static'),
        ({'darwin', 'g++'}, 'fbuild.builders.cxx.gxx.darwin.static'),
        ({'posix', 'clang++'}, 'fbuild.builders.cxx.clangxx.static'),
        ({'posix', 'g++'}, 'fbuild.builders.cxx.gxx.static'),
        ({'windows'}, 'fbuild.builders.cxx.msvc.static'),
    ), *args, **kwargs)

def guess_shared(*args, **kwargs):
    """L{shared} tries to guess the shared system c++ compiler according to the
    platform. It accepts a I{platform} keyword that overrides the system's
    platform. This can be used to use a non-default compiler. Any extra
    arguments and keywords are passed to the compiler's configuration
    functions."""

    return fbuild.builders.c._guess_builder('c++ shared', {'g++', 'clang++'}, (
        ({'iphone', 'simulator'},
            'fbuild.builders.cxx.gxx.iphone.shared_simulator'),
        ({'iphone'}, 'fbuild.builders.cxx.gxx.iphone.shared'),
        ({'darwin', 'clang++'}, 'fbuild.builders.cxx.clangxx.darwin.shared'),
        ({'darwin'}, 'fbuild.builders.cxx.gxx.darwin.shared'),
        ({'posix', 'clang++'}, 'fbuild.builders.cxx.clangxx.shared'),
        ({'posix'}, 'fbuild.builders.cxx.gxx.shared'),
        ({'windows'}, 'fbuild.builders.cxx.msvc.shared'),
    ), *args, **kwargs)
