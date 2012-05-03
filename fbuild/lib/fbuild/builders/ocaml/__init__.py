"""
Implement builders for the ocaml programming language.

This builder is compatible with ocaml 3.10 and above.
"""

import collections
import re
import os
from functools import partial
from itertools import chain

import fbuild
import fbuild.builders
import fbuild.builders.platform
import fbuild.db
import fbuild.temp
from fbuild.path import Path

# ------------------------------------------------------------------------------

class Ocamldep(fbuild.db.PersistentObject):
    """Use ocamldep to generate dependencies for ocaml files."""

    def __init__(self, ctx, exe=None, *, pre_flags=(), flags=()):
        super().__init__(ctx)

        self.exe = fbuild.builders.find_program(ctx,
            [exe] if exe else ['ocamldep.opt', 'ocamldep'])
        self.pre_flags = tuple(pre_flags)
        self.flags = tuple(flags)

    @fbuild.db.cachemethod
    def modules(self, src:fbuild.db.SRC, *,
            preprocessor=None,
            flags=()):
        """Calculate the modules this ocaml file depends on."""
        src = Path(src)

        cmd = [self.exe]
        cmd.extend(self.pre_flags)
        cmd.append('-modules')

        if preprocessor is not None:
            cmd.extend(('-pp', preprocessor))

        cmd.extend(self.flags)
        cmd.extend(flags)
        cmd.append(src)

        # Now, run ocamldep
        stdout, stderr = self.ctx.execute(cmd, str(self), src,
            color='yellow',
            stdout_quieter=1)

        # Parse the output and return the module dependencies.
        m = re.match(b'\S+:(?: (.*))?$', stdout.strip())
        if not m:
            raise fbuild.ExecutionError('unable to understand %r' % stdout)

        s = m.group(1)
        if s is None:
            return ()
        else:
            return tuple(s.decode().split())

    @fbuild.db.cachemethod
    def source_dependencies(self, src:fbuild.db.SRC, *,
            includes=(),
            **kwargs) -> fbuild.db.DSTS:
        """Compute the source files this ocaml file depends on."""
        deps = []

        def f(module, include):
            # On case-insensitive but case-preserving filesystems, we need to
            # be careful on how we deal with finding OCaml dependencies. Since
            # OCaml can store a module named List in either list.ml or List.ml,
            # we can't just test if the filename exists since fbuild needs to
            # deal with the exact filenames.  To do that, we'll grab the list
            # of filenames in the directory, then search for the right
            # spelling in that list.

            # Grab the filenames in the directory.
            if include is None:
                dirs = Path.getcwd().listdir()
            else:
                include = Path(include)

                if not include.exists():
                    # We can't search for dependencies in a directory that
                    # doesn't exist, so exit early.
                    return False

                dirs = include.listdir()

            found = False
            for suffix in '.mli', '.ml':
                # Look for the traditional lowercase form.
                path = module[0].lower() + module[1:] + suffix
                if path not in dirs:
                    # That didn't work, so lets try the uppercase form.
                    path = module[0].upper() + module[1:] + suffix
                    if path not in dirs:
                        # Couldn't find it, so just skip this module.
                        continue

                # We found it! Add that file to the dependencies.
                if include is None:
                    deps.append(Path(path))
                else:
                    deps.append(include / path)
                found = True

            return found

        modules = self.modules(src, **kwargs)
        for module in modules:
            if not f(module, None):
                for include in includes:
                    f(module, include)

        if src.endswith('.ml'):
            # The .mli file might not live right next to the .ml file, so
            # search the include path for it.
            mli = Path(src).replaceext('.mli')
            if mli.exists():
                deps.append(mli)
            else:
                # If we generated the .ml file, then perhaps there's a
                # pre-defined .mli file not in the buildroot.
                buildroot = kwargs.get('buildroot') or self.ctx.buildroot
                mli = mli.removeroot(buildroot + os.sep)

                if mli.exists():
                    deps.append(mli)
                else:
                    for include in includes:
                        path = mli.name
                        if include is not None: path = include / path
                        if path.exists():
                            deps.append(path)
                        break

        return deps

    def __str__(self):
        return self.exe.name

    def __repr__(self):
        return '%s(%r, %r, %r)' % (
            self.__class__.__name__,
            self.exe,
            self.pre_flags,
            self.flags)

# ------------------------------------------------------------------------------

class Builder(fbuild.builders.AbstractCompilerBuilder):
    def __init__(self, ctx, exe, *,
            platform=None,
            obj_suffix,
            lib_suffix,
            includes=(),
            libs=(),
            cc=None,
            c_libs=(),
            pre_flags=(),
            flags=(),
            debug=False,
            optimize=False,
            debug_flags=('-g',),
            optimize_flags=(),
            ocamldep=None,
            make_ocamldep=Ocamldep,
            requires_version=None,
            requires_at_least_version=None,
            requires_at_most_version=None):
        super().__init__(ctx, src_suffix='.ml')

        self.ocamldep = ocamldep or make_ocamldep(ctx)
        self.exe = exe
        self.obj_suffix = obj_suffix
        self.lib_suffix = lib_suffix
        self.exe_suffix = fbuild.builders.platform.exe_suffix(ctx, platform)
        self.includes = tuple(includes)
        self.libs = tuple(libs)
        self.cc = cc
        self.c_libs = tuple(c_libs)
        self.pre_flags = tuple(pre_flags)
        self.flags = tuple(flags)
        self.debug = debug
        self.optimize = optimize
        self.debug_flags = tuple(debug_flags)
        self.optimize_flags = tuple(optimize_flags)

        # Make sure we've got a valid version.
        fbuild.builders.check_version(ctx, self, self.version,
            requires_version=requires_version,
            requires_at_least_version=requires_at_least_version,
            requires_at_most_version=requires_at_most_version)

        # ----------------------------------------------------------------------
        # Check the builder to make sure it works.

        self.ctx.logger.check('checking if %s can make objects' % str(self))
        if self.try_compile():
            self.ctx.logger.passed()
        else:
            raise fbuild.ConfigFailed('%s compiler failed' % str(self))

        self.ctx.logger.check('checking if %s can make libraries' % str(self))
        if self.try_link_lib():
            self.ctx.logger.passed()
        else:
            raise fbuild.ConfigFailed('%s lib linker failed' % str(self))

        self.ctx.logger.check('checking if %s can make exes' % str(self))
        if self.try_link_exe():
            self.ctx.logger.passed()
        else:
            raise fbuild.ConfigFailed('%s exe linker failed' % str(self))

        self.ctx.logger.check('checking if %s can link lib to exe' % str(self))
        with fbuild.temp.tempdir() as parent:
            src_lib = parent / 'lib.ml'
            with open(src_lib, 'w') as f:
                print('let x = 5;;', file=f)

            src_exe = parent / 'exe.ml'
            with open(src_exe, 'w') as f:
                print('print_int Lib.x;;', file=f)

            obj = self.uncached_compile(src_lib, quieter=1)
            lib = self.uncached_link_lib(parent / 'lib', [obj], quieter=1)

            obj = self.uncached_compile(src_exe, quieter=1)
            exe = self.uncached_link_exe(parent / 'exe', [obj], libs=[lib],
                quieter=1)

            try:
                stdout, stderr = self.ctx.execute([exe], quieter=1)
            except fbuild.ExecutionError:
                raise fbuild.ConfigFailed('failed to link %s lib to exe' %
                    str(self))
            else:
                if stdout != b'5':
                   raise fbuild.ConfigFailed('failed to link %s lib to exe' %
                        str(self))
                self.ctx.logger.passed()

    # --------------------------------------------------------------------------

    def where(self):
        stdout, stderr = self.ctx.execute([self.exe, '-where'], quieter=1)
        return Path(stdout.decode().strip())

    def version(self):
        """Return the version of the ocaml executable."""
        stdout, stderr = self.ctx.execute([self.exe, '-version'], quieter=1)
        return stdout.decode().strip()

    # --------------------------------------------------------------------------

    def _run(self, dst, srcs, *,
            includes=(),
            libs=(),
            external_libs=(),
            pre_flags=(),
            flags=(),
            debug=None,
            optimize=None,
            custom=False,
            cc=None,
            c_libs=(),
            for_pack=None,
            preprocessor=None,
            buildroot=None,
            **kwargs):
        buildroot = buildroot or self.ctx.buildroot
        libs = tuple(chain(self.libs, external_libs, libs))

        # we need to make sure libraries are built first before we compile
        # the sources
        assert srcs or libs, "%s: no sources or libraries passed in" % dst

        dst = Path(dst).addroot(buildroot)
        dst.parent.makedirs()

        extra_srcs = []
        for lib in libs:
            if Path(lib).exists():
                extra_srcs.append(lib)
            else:
                extra_srcs.append(lib + self.lib_suffix)

        cmd = [self.exe]
        cmd.extend(self.pre_flags)
        cmd.extend(pre_flags)

        if (debug is None and self.debug) or debug:
            cmd.extend(self.debug_flags)

        if (optimize is None and self.optimize) or optimize:
            cmd.extend(self.optimize_flags)

        includes = set(includes)
        includes.update(self.includes)
        includes.add(dst.parent)

        for i in sorted(includes):
            i = Path(i)
            if i.exists():
                cmd.extend(('-I', i))

        if custom:
            cmd.append('-custom')

        cc = cc or self.cc
        if cc:
            cmd.extend(('-cc', cc))

        for lib in chain(self.c_libs, c_libs):
            if Path(lib).exists():
                cmd.extend(('-cclib', lib))
            else:
                cmd.extend(('-cclib', '-l' + lib))

        if preprocessor is not None:
            cmd.extend(('-pp', preprocessor))

        if for_pack is not None:
            cmd.extend(('-for-pack', for_pack))

        cmd.extend(self.flags)
        cmd.extend(flags)
        cmd.extend(('-o', dst))
        cmd.extend(extra_srcs)
        cmd.extend(srcs)

        self.ctx.execute(cmd, str(self),
            '%s -> %s' % (' '.join(extra_srcs + srcs), dst),
            **kwargs)

        return dst

    # --------------------------------------------------------------------------

    @fbuild.db.cachemethod
    def compile(self, src:fbuild.db.SRC, *args,
            includes=(),
            ocamldep_flags=(),
            preprocessor=None,
            **kwargs) -> fbuild.db.DST:
        """Compile an ocaml implementation or interface file and cache the
        results."""
        dst = self.uncached_compile(src, *args,
            includes=includes,
            preprocessor=preprocessor,
            **kwargs)
        self._add_compile_dependencies(dst, src,
            includes=includes,
            preprocessor=preprocessor,
            flags=ocamldep_flags)
        return dst


    def uncached_compile(self, src, dst=None, *args,
            buildroot=None,
            pre_flags=(),
            **kwargs):
        """Compile an ocaml implementation or interface file without caching
        the results.  This is needed when compiling temporary files."""
        if src.endswith('.mli'):
            obj_suffix = '.cmi'
        else:
            obj_suffix = self.obj_suffix

        # Copy the source into the buildroot as if we generate a .ml, the .mli
        # needs to also be in the buildroot for ocaml to use the .cmi file.
        buildroot = buildroot or self.ctx.buildroot
        src_buildroot = src.addroot(buildroot)
        if src != src_buildroot:
            src_buildroot.parent.makedirs()
            src.copy(src_buildroot)
            src = src_buildroot

        dst = Path(dst or src).replaceext(obj_suffix)

        pre_flags = list(pre_flags)
        pre_flags.append('-c')

        return self._run(dst, [src],
            pre_flags=pre_flags,
            color='compile',
            buildroot=buildroot,
            *args, **kwargs)

    def _add_compile_dependencies(self, dst, src, **kwargs):
        # If the .mli file doesn't exist, ocaml creates one from the .ml file
        if src.endswith('.ml') and not src.replaceext('.mli').exists():
            dsts = (dst.replaceext('.cmi'),)
        else:
            dsts = ()

        self.ctx.db.add_external_dependencies_to_call(
            srcs=self.scan(src, **kwargs),
            dsts=dsts)

    # --------------------------------------------------------------------------

    @fbuild.db.cachemethod
    def link_lib(self, dst, srcs:fbuild.db.SRCS, *args,
            libs:fbuild.db.SRCS=(),
            **kwargs) -> fbuild.db.DST:
        """Link all the L{srcs} into a library."""
        return self._link(self.uncached_link_lib, dst, srcs, *args,
            libs=libs,
            **kwargs)

    @fbuild.db.cachemethod
    def link_exe(self, dst, srcs:fbuild.db.SRCS, *args,
            libs:fbuild.db.SRCS=(),
            **kwargs) -> fbuild.db.DST:
        """Link all the L{srcs} into an executable."""
        return self._link(self.uncached_link_exe, dst, srcs, *args,
            libs=libs,
            **kwargs)

    def _link(self, function, dst, srcs, *args, includes, libs, **kwargs):
        dst = function(dst, srcs, *args,
            includes=includes,
            libs=libs,
            **kwargs)
        self._add_link_dependencies(dst, srcs, includes, libs)
        return dst

    def _add_link_dependencies(self, dst, srcs, includes, libs):
        # Do nothing
        pass

    # --------------------------------------------------------------------------

    def uncached_link_lib(self, dst, *args,
            libs=(),
            external_libs=(),
            pre_flags=(),
            **kwargs):
        """Link compiled ocaml files into a library without caching the
        results.  This is needed when linking temporary files."""
        # ignore passed in libraries
        return self._uncached_link(dst + self.lib_suffix,
            pre_flags=tuple(chain(('-a',), pre_flags)), *args, **kwargs)

    def uncached_link_exe(self, dst, *args, **kwargs):
        """Link compiled ocaml files into an executable without caching the
        results.  This is needed when linking temporary files."""
        return self._uncached_link(dst + self.exe_suffix, *args, **kwargs)

    def _uncached_link(self, dst, srcs, *args, libs=(), **kwargs):
        """Actually link the sources."""
        # Filter out the .cmi files, such as when we're using ocamlyacc source
        # files.
        srcs = [src for src in srcs if not src.endswith('.cmi')]

        return self._run(dst, srcs, libs=libs, color='link', *args, **kwargs)

    # --------------------------------------------------------------------------

    @fbuild.db.cachemethod
    def build_objects(self, srcs:fbuild.db.SRCS, *,
            includes=(),
            libs=(),
            external_libs=(),
            buildroot=None,
            ocamldep_flags=(),
            preprocessor=None,
            **kwargs) -> fbuild.db.DSTS:
        """Compile all the L{srcs} in parallel."""
        kwargs['buildroot'] = buildroot = buildroot or self.ctx.buildroot
        kwargs['includes']  = includes  = set(includes)

        # Add the library parents to the search paths
        includes.update(lib.parent for lib in libs)
        includes.update(lib.parent for lib in external_libs)

        srcs = [Path(src) for src in srcs]
        for src in srcs:
            parent = src.parent
            if parent:
                includes.add(parent)
                includes.add(parent.addroot(self.ctx.buildroot))
            else:
                # We're at the toplevel directory, so just add the buildroot to
                # the includes.
                includes.add(self.ctx.buildroot)

        # Add additional source dependencies to the call.
        deps = set()
        for src in srcs:
            deps.update(self.scan(src,
                includes=includes,
                preprocessor=preprocessor,
                flags=ocamldep_flags))

        if deps:
            self.ctx.db.add_external_dependencies_to_call(srcs=deps)

        objs = self.ctx.scheduler.map_with_dependencies(
            partial(self.ocamldep.source_dependencies,
                includes=includes,
                preprocessor=preprocessor,
                flags=ocamldep_flags),
            partial(self.compile,
                ocamldep_flags=ocamldep_flags,
                preprocessor=preprocessor,
                **kwargs),
            srcs)

        self.ctx.db.add_external_dependencies_to_call(
            dsts=(obj.replaceext('.cmi') for obj in objs))

        return objs

    # --------------------------------------------------------------------------

    @fbuild.db.cachemethod
    def build_pack(self, dst, srcs:fbuild.db.SRCS, *,
            buildroot=None,
            objs=(),
            pre_flags=(),
            **kwargs) -> fbuild.db.DST:
        """Compile all the L{srcs} into a packed object."""

        buildroot = buildroot or self.ctx.buildroot
        dst = Path(dst).addroot(buildroot) + self.obj_suffix
        dst.parent.makedirs()

        for_pack = dst.name.splitext()[0]
        for_pack = for_pack[0].upper() + for_pack[1:]

        objs = list(objs)
        objs = self.build_objects(srcs,
            for_pack=for_pack,
            pre_flags=pre_flags,
            **kwargs)

        # We need to filter out any .cmi files.
        objs = [obj for obj in objs if not obj.endswith('.cmi')]

        pre_flags = list(pre_flags)
        pre_flags.append('-pack')

        dst = self._run(dst, objs,
            color='link',
            pre_flags=pre_flags,
            **kwargs)

        self.ctx.db.add_external_dependencies_to_call(
            srcs=objs + [obj.replaceext('.cmi') for obj in objs],
            dsts=[dst, dst.replaceext('.cmi')])

        return dst

    # --------------------------------------------------------------------------

    def build_lib(self, dst, srcs:fbuild.db.SRCS, *args,
            **kwargs) -> fbuild.db.DST:
        """Compile all the L{srcs} and link into a library."""
        return self._build_link(self.link_lib, dst, srcs, *args,
            **kwargs)

    def build_exe(self, dst, srcs:fbuild.db.SRCS, *args,
            libs:fbuild.db.SRCS=(),
            **kwargs) -> fbuild.db.DST:
        """Compile all the L{srcs} and link into an executable."""
        return self._build_link(self.link_exe, dst, srcs, *args,
            libs=libs,
            **kwargs)

    # --------------------------------------------------------------------------

    def _build_link(self, function, dst, srcs, *,
            objs=(),
            pack=None,
            includes=(),
            cflags=(),
            ckwargs={},
            libs=(),
            external_libs=(),
            custom=False,
            c_libs=(),
            lflags=(),
            lkwargs={},
            flags=(),
            buildroot=None,
            **kwargs):
        # This must be called from a cached function to work properly.
        buildroot = buildroot or self.ctx.buildroot
        includes = set(includes)
        for lib in libs:
            if isinstance(lib, Path):
                includes.add(lib.parent)
                includes.add(lib.parent.removeroot(buildroot + os.sep))

        if pack is None:
            objs = list(objs)
            objs.extend(self.build_objects(srcs,
                includes=includes,
                flags=list(chain(flags, cflags)),
                buildroot=buildroot,
                **dict(kwargs, **ckwargs)))
        else:
            objs = [self.build_pack(pack, srcs,
                objs=objs,
                includes=includes,
                flags=list(chain(flags, cflags)),
                buildroot=buildroot,
                **dict(kwargs, **ckwargs))]

        return function(dst, objs,
            includes=includes,
            libs=libs,
            external_libs=external_libs,
            custom=custom,
            c_libs=c_libs,
            flags=list(chain(flags, lflags)),
            buildroot=buildroot,
            **dict(kwargs, **lkwargs))

    # --------------------------------------------------------------------------

    def __str__(self):
        return self.exe.name

    def __repr__(self):
        return '%s(%r)' % (self.__class__.__name__, self.exe)

# ------------------------------------------------------------------------------

class BytecodeBuilder(Builder):
    def __init__(self, ctx, exe, *args,
            obj_suffix='.cmo',
            lib_suffix='.cma',
            **kwargs):
        super().__init__(ctx, exe, *args,
            obj_suffix=obj_suffix,
            lib_suffix=lib_suffix,
            **kwargs)

    @fbuild.db.cachemethod
    def scan(self, src:fbuild.db.SRC, *,
            includes=(),
            **kwargs) -> fbuild.db.DSTS:
        """Recursively compute all the source files this ocaml file depends
        on."""
        lookup = {}
        for dep in self.ocamldep.source_dependencies(src,
                includes=includes,
                **kwargs):
            base, ext = dep.splitext()
            if ext == '.mli' or base not in lookup:
                lookup[base] = dep

        deps = set()
        for dep in lookup.values():
            deps.add(dep)
            d = self.scan(dep, includes=includes, **kwargs)
            deps.update(d)

        return deps

# ------------------------------------------------------------------------------

class Ocamlc(BytecodeBuilder):
    def __init__(self, ctx, exe=None, *args, **kwargs):
        exe = fbuild.builders.find_program(ctx,
            [exe] if exe else ['ocamlc.opt', 'ocamlc'])

        super().__init__(ctx, exe, *args, **kwargs)

# ------------------------------------------------------------------------------

class Ocamlcp(BytecodeBuilder):
    def __init__(self, ctx, exe=None, *args, profile_flags=(), **kwargs):
        exe = fbuild.builders.find_program(ctx,
            [exe] if exe else ['ocamlcp.opt', 'ocamlcp'])

        self.profile_flags = tuple(profile_flags)

        super().__init__(ctx, exe, *args, **kwargs)

    def _run(self, *args, flags=(), profile_flags=None, **kwargs):
        """Add the profile flags."""
        if profile_flags is None:
            profile_flags = self.profile_flags

        return super()._run(*args, flags=flags + profile_flags, **kwargs)

# ------------------------------------------------------------------------------

class Ocamlopt(Builder):
    def __init__(self, ctx, exe=None, *args,
            platform=None,
            obj_suffix='.cmx',
            lib_suffix='.cmxa',
            ocamldep=None,
            make_ocamldep=Ocamldep,
            ocamlc=None,
            make_ocamlc=Ocamlc,
            profile=False,
            profile_flags=('-p',),
            **kwargs):
        # We need the bytecode compiler to compile .mli files.
        # Note we purposely don't pass the profile to ocamlc.
        self.ocamlc = ocamlc or make_ocamlc(ctx,
            ocamldep=ocamldep,
            make_ocamldep=make_ocamldep)

        self.native_obj_suffix = \
            fbuild.builders.platform.obj_suffix(ctx, platform)
        self.native_lib_suffix = \
            fbuild.builders.platform.static_lib_suffix(ctx, platform)

        self.profile = profile
        self.profile_flags = tuple(profile_flags)

        exe = fbuild.builders.find_program(ctx,
            [exe] if exe else ['ocamlopt.opt', 'ocamlopt'])
        super().__init__(ctx, exe, *args,
            platform=platform,
            obj_suffix=obj_suffix,
            lib_suffix=lib_suffix,
            ocamldep=ocamldep,
            make_ocamldep=make_ocamldep,
            **kwargs)

    # --------------------------------------------------------------------------

    @fbuild.db.cachemethod
    def scan(self, src:fbuild.db.SRC, *,
            includes=(),
            **kwargs) -> fbuild.db.DSTS:
        """Recursively compute all the source files this ocaml file depends
        on."""
        deps = set()
        def f(src):
            for dep in self.ocamldep.source_dependencies(src,
                    includes=includes,
                    **kwargs):
                # Don't scan this dependency if we've already scanned it.
                if dep not in deps:
                    deps.add(dep)
                    f(dep)

        f(src)

        return deps

    # --------------------------------------------------------------------------

    def _run(self, *args, flags=(), profile=None, profile_flags=None, **kwargs):
        """Add the profile flags if requested."""
        if (profile is None and self.profile) or profile:
            if profile_flags is None:
                profile_flags = self.profile_flags

            flags = tuple(chain(flags, self.profile_flags))

        return super()._run(*args, flags=flags, **kwargs)

    # --------------------------------------------------------------------------

    def compile(self, src, *args, **kwargs):
        """Compile an ocaml implementation or interface file and cache the
        results."""
        # If the src is an interface file, use the bytecode compiler to create
        # the .cmi file.
        if src.endswith('.mli'):
            return self.ocamlc.compile(src, *args, **kwargs)
        else:
            return super().compile(src, *args, **kwargs)

    def uncached_compile(self, src, *args, **kwargs):
        # If the src is an interface file, use the bytecode compiler to create
        # the .cmi file.
        if src.endswith('.mli'):
            return self.ocamlc.uncached_compile(src, *args, **kwargs)
        else:
            return super().uncached_compile(src, *args, **kwargs)

    # --------------------------------------------------------------------------

    def _add_compile_dependencies(self, dst, src, **kwargs):
        super()._add_compile_dependencies(dst, src, **kwargs)

        self.ctx.db.add_external_dependencies_to_call(
            dsts=(dst.replaceext(self.native_obj_suffix),))

    def _add_link_dependencies(self, dst, srcs, includes, libs):
        super()._add_link_dependencies(dst, srcs, includes, libs)

        # ocamlopt also produces native objects and libraries, so add
        # additional dependencies on them.
        srcs = chain(
            (src.replaceext(self.native_obj_suffix) for src in srcs
                if src.endswith('.cmx')),
            (lib.replaceext(self.native_lib_suffix) for lib in libs))

        # If the dst is a library, this cached call also depends on the native
        # library.
        if dst.endswith('.cmxa'):
            dsts = (dst.replaceext(self.native_lib_suffix),)
        else:
            dsts = ()

        # This function must be called by a cached function.
        self.ctx.db.add_external_dependencies_to_call(
            srcs=srcs,
            dsts=dsts)

# ------------------------------------------------------------------------------

class Ocaml(fbuild.builders.AbstractCompilerBuilder):
    Tuple = collections.namedtuple('Tuple', 'bytecode native')

    def __init__(self, ctx, *,
            ocamldep=None,
            ocamlc=None,
            ocamlcp=None,
            ocamlopt=None,
            make_ocamldep=Ocamldep,
            make_ocamlc=Ocamlc,
            make_ocamlcp=Ocamlcp,
            make_ocamlopt=Ocamlopt,
            profile=False,
            **kwargs):
        self.ocamldep = ocamldep or make_ocamldep(ctx)

        # ocamlc doesn't take profile flags.
        self.ocamlc = make_ocamlc(ctx,
            ocamldep=ocamldep,
            exe=ocamlc,
            make_ocamldep=make_ocamldep,
            **kwargs)

        self.ocamlcp = make_ocamlcp(ctx,
            ocamldep=ocamldep,
            exe=ocamlcp,
            make_ocamldep=make_ocamldep,
            **kwargs)

        self.ocamlopt = make_ocamlopt(ctx,
            ocamldep=self.ocamldep,
            ocamlc=self.ocamlc,
            exe=ocamlopt,
            make_ocamldep=make_ocamldep,
            make_ocamlc=make_ocamlc,
            profile=profile,
            **kwargs)

    # --------------------------------------------------------------------------

    def compile(self, *args, **kwargs):
        """Compile an ocaml implementation or interface file and cache the
        results.  This returns a tuple of the generated bytecode and native
        object filename."""
        # The sub-compilers will handle the actual caching.
        return self._compile(self.ocamlc.compile, self.ocamlopt.compile,
            *args, **kwargs)

    def uncached_compile(self, *args, **kwargs):
        """Compile an ocaml implementation or interface file without caching
        the results.  This is needed when compiling temporary files. This
        returns a tuple of the generated bytecode and native object
        filename."""
        return self._compile(
            self.ocamlc.uncached_compile, self.ocamlopt.uncached_compile,
            *args, **kwargs)

    def link_lib(self, *args, **kwargs):
        """Link compiled ocaml files into a bytecode and native library and
        cache the results."""
        # The sub-linkers will handle the actual caching.
        return self._link(self.ocamlc.link_lib, self.ocamlopt.link_lib,
            *args, **kwargs)

    def uncached_link_lib(self, *args, **kwargs):
        """Link compiled ocaml files into a bytecode and native library without
        caching the results.  This is needed when linking temporary files."""
        return self._link(
            self.ocamlc.uncached_link_lib, self.ocamlopt.uncached_link_lib,
            *args, **kwargs)

    def link_exe(self, *args, **kwargs):
        """Link compiled ocaml files into a bytecode and native executable and
        cache the results."""
        # The sub-linkers will handle the actual caching.
        return self._link(self.ocamlc.link_exe, self.ocamlopt.link_exe,
            *args, **kwargs)

    def uncached_link_exe(self, *args, **kwargs):
        """Link compiled ocaml files into a bytecode and native executable
        without caching the results.  This is needed when linking temporary
        files."""
        return self._link(
            self.ocamlc.uncached_link_exe, self.ocamlopt.uncached_link_exe,
            *args, **kwargs)

    # --------------------------------------------------------------------------

    def build_objects(self, srcs, *args, **kwargs):
        """Compile all the L{srcs} in parallel."""
        return \
            self.ocamlc.build_objects(srcs, *args, **kwargs) + \
            self.ocamlopt.build_objects(srcs, *args, **kwargs)

    def build_lib(self, *args, **kwargs):
        """Compile and link ocaml source files into a bytecode and native
        library."""
        return self._link(
            self.ocamlc.build_lib,
            self.ocamlopt.build_lib, *args, **kwargs)

    def build_exe(self, *args, **kwargs):
        """Compile and link ocaml source files into a bytecode and native
        executable."""
        return self._link(
            self.ocamlc.build_exe,
            self.ocamlopt.build_exe, *args, **kwargs)

    # --------------------------------------------------------------------------

    def _compile(self, bcompile, ncompile, *args, **kwargs):
        """Actually compile the source using the bytecode and native
        compilers."""
        bobj = bcompile(*args, **kwargs)

        if src.endswith('.mli'):
            # We only need to generate the interface once.
            nobj = bobj
        else:
            nobj = ncompile(*args, **kwargs)

        return self.Tuple(bobj, nobj)

    def _link(self, blink, nlink, dst, srcs, *args,
            libs=(),
            custom=False,
            **kwargs):
        """Actually link the sources using the bytecode and native compilers."""
        # the first item is the bytecode object, the second the native one
        bsrcs = [(s[0] if isinstance(s, self.Tuple) else s) for s in srcs]
        nsrcs = [(s[1] if isinstance(s, self.Tuple) else s) for s in srcs]

        # the first item is the bytecode lib, the second the native one
        blibs = [(l[0] if isinstance(l, self.Tuple) else l) for l in libs]
        nlibs = [(l[1] if isinstance(l, self.Tuple) else l) for l in libs]

        blib = blink(dst + '.byte', bsrcs, *args,
            libs=blibs,
            custom=custom, **kwargs)
        nlib = nlink(dst + '.native', nsrcs, *args,
            libs=nlibs,
            **kwargs)

        return self.Tuple(blib, nlib)

# ------------------------------------------------------------------------------

class Ocamllex(fbuild.db.PersistentObject):
    def __init__(self, ctx, exe=None, flags=()):
        super().__init__(ctx)

        self.exe = fbuild.builders.find_program(ctx,
            [exe] if exe else ['ocamllex.opt', 'ocamllex'])
        self.flags = flags

    @fbuild.db.cachemethod
    def __call__(self, src:fbuild.db.SRC, *,
            flags=(),
            buildroot=None) -> fbuild.db.DST:
        buildroot = buildroot or self.ctx.buildroot
        dst = src.replaceext('.ml').addroot(buildroot)
        dst.parent.makedirs()

        cmd = [self.exe]
        cmd.extend(('-o', dst))
        cmd.extend(self.flags)
        cmd.extend(flags)
        cmd.append(src)

        self.ctx.execute(cmd, str(self),
            '%s -> %s' % (src, dst),
            color='yellow')

        return dst

    def __str__(self):
        return self.exe.name

    def __repr__(self):
        return '%s(%r, %r)' % (self.__class__.__name__, self.exe, self.flags)

# ------------------------------------------------------------------------------

class Ocamlyacc(fbuild.db.PersistentObject):
    def __init__(self, ctx, exe=None, flags=()):
        super().__init__(ctx)

        self.exe = fbuild.builders.find_program(ctx,
            [exe] if exe else ['ocamlyacc.opt', 'ocamlyacc'])
        self.flags = flags

    @fbuild.db.cachemethod
    def __call__(self, src:fbuild.db.SRC, *,
            flags=(),
            buildroot=None) -> fbuild.db.DSTS:
        buildroot = buildroot or self.ctx.buildroot
        # first, copy the src file into the buildroot
        src_buildroot = src.addroot(buildroot)
        dsts = (
            src_buildroot.replaceext('.ml'),
            src_buildroot.replaceext('.mli'),
        )

        if src != src_buildroot:
            src_buildroot.parent.makedirs()
            src.copy(src_buildroot)
            src = src_buildroot

        cmd = [self.exe]
        cmd.extend(self.flags)
        cmd.extend(flags)
        cmd.append(src)

        self.ctx.execute(cmd, str(self), '%s -> %s' % (src, ' '.join(dsts)),
            color='yellow')

        return dsts

    def __str__(self):
        return self.exe.name

    def __repr__(self):
        return '%s(%r, %r)' % (self.__class__.__name__, self.exe, self.flags)
