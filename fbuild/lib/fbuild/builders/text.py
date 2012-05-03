import collections
import re

import fbuild
import fbuild.db

# ------------------------------------------------------------------------------

@fbuild.db.caches
def substitute(ctx, dst, src:fbuild.db.SRC, patterns, *, buildroot=None) \
        -> fbuild.db.DST:
    """L{substitute} replaces the I{patterns} in the file named I{src}
    and saves the changes into file named I{dst}."""

    buildroot = buildroot or ctx.buildroot
    src = fbuild.path.Path(src)
    dst = fbuild.path.Path.addroot(dst, buildroot)
    dst.parent.makedirs()

    ctx.logger.log(' * creating ' + dst, color='yellow')

    with open(src, 'r') as src_file:
        code = src_file.read()
        for pattern, text in patterns.items():
            code = code.replace(pattern, text)

    with open(dst, 'w') as dst_file:
        dst_file.write(code)

    return dst

# ------------------------------------------------------------------------------

@fbuild.db.caches
def regex_substitute(ctx, dst, src:fbuild.db.SRC, patterns, *,
        buildroot=None) -> fbuild.db.DST:
    """L{substitute} replaces the I{patterns} in the file named I{src}
    and saves the changes into file named I{dst}."""

    buildroot = buildroot or ctx.buildroot
    src = fbuild.path.Path(src)
    dst = fbuild.path.Path.addroot(dst, buildroot)
    dst.parent.makedirs()

    ctx.logger.log(' * creating ' + dst, color='yellow')

    with open(src, 'r') as src_file:
        code = src_file.read()
        for items in patterns:
            try:
                pattern, text, flags = items
            except ValueError:
                pattern, text = items
                flags = re.M

            code = re.sub(pattern, text, code, flags=flags)

    with open(dst, 'w') as dst_file:
        dst_file.write(code)

    return dst

# ------------------------------------------------------------------------------

@fbuild.db.caches
def format_substitute(ctx, dst, src:fbuild.db.SRC, patterns, *,
        buildroot=None) -> fbuild.db.DST:
    """L{format_substitute} replaces the I{patterns} in the file named I{src}
    and saves the changes into file named I{dst}. It uses python's format
    patterns for finding the insertion points."""

    buildroot = buildroot or ctx.buildroot
    src = fbuild.path.Path(src)
    dst = fbuild.path.Path.addroot(dst, buildroot)
    dst.parent.makedirs()

    ctx.logger.log(' * creating ' + dst, color='yellow')

    with open(src, 'r') as src_file:
        code = src_file.read().format(**patterns)

    with open(dst, 'w') as dst_file:
        dst_file.write(code)

    return dst

# ------------------------------------------------------------------------------

@fbuild.db.caches
def autoconf_config_file(ctx, dst, src:fbuild.db.SRC, patterns, *,
        buildroot=None) -> fbuild.db.DST:
    """L{autoconf_config_file} replaces the I{patterns} in the file named
    I{src} and saves the changes into file named I{dst}. It uses autoconf
    AC_CONFIG_FILES @word@ patterns to find the insertion points."""

    buildroot = buildroot or ctx.buildroot
    src = fbuild.path.Path(src)
    dst = fbuild.path.Path(dst).addroot(buildroot)
    dst.parent.makedirs()

    ctx.logger.log(' * creating ' + dst, color='yellow')

    def replace(match):
        value = patterns[match.group(1)]
        if isinstance(value, str):
            return value
        elif isinstance(value, collections.Iterable):
            return ' '.join(str(v) for v in value)
        return str(value)

    with open(src, 'r') as src_file:
        code = src_file.read()

    code = re.sub('@(\w+)@', replace, code, flags=re.M)

    with open(dst, 'w') as dst_file:
        dst_file.write(code)

    return dst

# ------------------------------------------------------------------------------

@fbuild.db.caches
def autoconf_config_header(ctx, dst, src:fbuild.db.SRC, patterns, *,
        buildroot=None) -> fbuild.db.DST:
    """L{autoconf_config_header} replaces the I{patterns} in the file named
    I{src} and saves the changes into file named I{dst}. It uses autoconf
    AC_CONFIG_HEADERS @word@ and #define patterns to find the insertion
    points."""

    buildroot = buildroot or ctx.buildroot
    src = fbuild.path.Path(src)
    dst = fbuild.path.Path.addroot(dst, buildroot)
    dst.parent.makedirs()

    ctx.logger.log(' * creating ' + dst, color='yellow')

    missing_definitions = []

    def replace(match):
        if match.group('sub'):
            # Handle the @foo@ substitution
            value = patterns[match.group('sub')]
            if isinstance(value, str):
                return value
            elif isinstance(value, collections.Iterable):
                return ' '.join(str(v) for v in value)
            return str(value)
        else:
            # Handle the #undef replacement
            key = match.group('def')
            try:
                value = patterns[key]
            except KeyError:
                # We couldn't find a value for this
                # key, so log it and continue on.
                missing_definitions.append(key)
                value = None

            if isinstance(value, bool):
                value = int(value)
            elif \
                    not isinstance(value, str) and \
                    isinstance(value, collections.Iterable):
                value = ' '.join(str(v) for v in value)

            if value:
                return '#define %s %s' % (key, value)
            else:
                return '/* #undef %s */' % (key)

    with open(src, 'r') as src_file:
        code = src_file.read()

    code = re.sub('(^#undef +(?P<def>\w+)$)|(?:@(?P<sub>\w+)@)', replace, code,
        flags=re.M)

    if missing_definitions:
        raise fbuild.Error('missing definitions: %s' %
            ' '.join(missing_definitions))

    with open(dst, 'w') as dst_file:
        dst_file.write(code)

    return dst
