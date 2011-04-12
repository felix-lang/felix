import fbuild.functools

# ------------------------------------------------------------------------------

baked_configs = [
    ({'iphone'}, 'buildsystem.config.iphone.'),
]

def config_call(function, platform, *args, **kwargs):
    """Check a cache of configurations to see if we've hardcoded the
    configuration of a function for a specific platform."""

    if function.startswith('fbuild.config.'):
        for subplatform, module in baked_configs:
            if subplatform <= platform:
                name = module + \
                    function[len('fbuild.config.'):].replace('.', '_')
                try:
                    f = fbuild.functools.import_function(name)
                except KeyError:
                    pass
                else:
                    return f(*args, **kwargs)

    return fbuild.functools.call(function, *args, **kwargs)
