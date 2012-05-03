import plistlib
from itertools import chain

import fbuild.builders.c.gcc.darwin as darwin
from fbuild.path import Path

# ------------------------------------------------------------------------------

def _iphone_devroot(simulator):
    if simulator:
        return Path('/Developer/Platforms/iPhoneSimulator.platform/Developer')
    else:
        return Path('/Developer/Platforms/iPhoneOS.platform/Developer')

def _iphone_sdkroot(sdk, simulator):
    devroot = _iphone_devroot(simulator)

    if sdk is None:
        if simulator:
            info = plistlib.readPlist(
                '/Developer/Platforms/iPhoneSimulator.platform/Info.plist')
            sdk = 'iPhoneSimulator%s.sdk' % info['CFBundleShortVersionString']
        else:
            info = plistlib.readPlist(
                '/Developer/Platforms/iPhoneOS.platform/Info.plist')
            sdk = 'iPhoneOS%s.sdk' % info['CFBundleShortVersionString']

    return devroot / 'SDKs' / sdk

def _builder(builder, *args, pre_flags=[],
        sdk=None,
        arch=None,
        machine_flags=(),
        simulator,
        **kwargs):
    pre_flags = list(pre_flags)
    pre_flags.extend(('-isysroot', _iphone_sdkroot(sdk, simulator)))

    if simulator:
        machine_flags = tuple(chain(('32',), machine_flags))
    elif arch is None:
        arch = 'armv6'

    return builder(*args,
        pre_flags=pre_flags,
        arch=arch,
        machine_flags=machine_flags,
        **kwargs)

# ------------------------------------------------------------------------------

def static(ctx, exe=None, *args, **kwargs):
    if exe is None:
        exe = _iphone_devroot(False) / 'usr/bin/gcc'

    return _builder(darwin.static, ctx, exe, *args,
        simulator=False,
        cross_compiler=True,
        **kwargs)

def shared(ctx, exe=None, *args, **kwargs):
    if exe is None:
        exe = _iphone_devroot(False) / 'usr/bin/gcc'

    return _builder(darwin.shared, ctx, exe, *args,
        simulator=False,
        cross_compiler=True,
        **kwargs)

# ------------------------------------------------------------------------------

def static_simulator(ctx, exe=None, *args, **kwargs):
    if exe is None:
        exe = _iphone_devroot(True) / 'usr/bin/gcc'

    return _builder(darwin.static, ctx, exe, *args, simulator=True, **kwargs)

def shared_simulator(ctx, exe=None, *args, **kwargs):
    if exe is None:
        exe = _iphone_devroot(True) / 'usr/bin/gcc'

    return _builder(darwin.shared, ctx, exe, *args, simulator=True, **kwargs)
