import fbuild.builders.c

# ------------------------------------------------------------------------------

def guess_static(*args, **kwargs):
    """L{static} tries to guess the static system c++ compiler according to the
    platform. It accepts a I{platform} keyword that overrides the system's
    platform. This can be used to use a non-default compiler. Any extra
    arguments and keywords are passed to the compiler's configuration
    functions."""

    return fbuild.builders.c._guess_builder('c++ static',
        {'g++', 'clang++', 'icpc'}, (
        ({'windows'}, 'fbuild.builders.cxx.msvc.static'),
        ({'iphone', 'simulator', 'g++'},
            'fbuild.builders.cxx.gxx.iphone.static_simulator'),
        ({'iphone', 'g++'}, 'fbuild.builders.cxx.gxx.iphone.static'),
        ({'darwin', 'clang++'}, 'fbuild.builders.cxx.clangxx.darwin.static'),
        ({'darwin', 'g++'}, 'fbuild.builders.cxx.gxx.darwin.static'),
        ({'clang++'}, 'fbuild.builders.cxx.clangxx.static'),
        ({'icpc'}, 'fbuild.builders.cxx.intelxx.static'),
        ({'g++'}, 'fbuild.builders.cxx.gxx.static'),
    ), *args, **kwargs)

def guess_shared(*args, **kwargs):
    """L{shared} tries to guess the shared system c++ compiler according to the
    platform. It accepts a I{platform} keyword that overrides the system's
    platform. This can be used to use a non-default compiler. Any extra
    arguments and keywords are passed to the compiler's configuration
    functions."""

    return fbuild.builders.c._guess_builder('c++ shared',
        {'g++', 'clang++', 'icpc'}, (
        ({'windows'}, 'fbuild.builders.cxx.msvc.shared'),
        ({'iphone', 'simulator'},
            'fbuild.builders.cxx.gxx.iphone.shared_simulator'),
        ({'iphone'}, 'fbuild.builders.cxx.gxx.iphone.shared'),
        ({'darwin', 'clang++'}, 'fbuild.builders.cxx.clangxx.darwin.shared'),
        ({'darwin'}, 'fbuild.builders.cxx.gxx.darwin.shared'),
        ({'clang++'}, 'fbuild.builders.cxx.clangxx.shared'),
        ({'icpc'}, 'fbuild.builders.cxx.intelxx.shared'),
        ({'g++'}, 'fbuild.builders.cxx.gxx.shared'),
    ), *args, **kwargs)
