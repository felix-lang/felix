import difflib
from itertools import chain

import fbuild
import fbuild.db
from fbuild.functools import call
from fbuild.path import Path
import os
import os.path
import platform

# ------------------------------------------------------------------------------

def _getcwd():
  cwd = os.getcwd().replace(":", "\\")
  if cwd[0] != os.sep: cwd = os.sep + cwd 
  return cwd
  # absolute filename in Unix
  # \C\blah on Windows

# ------------------------------------------------------------------------------

class Builder(fbuild.db.PersistentObject):
    def __init__(self, ctx, flxg, cxx,
            flx_run_lib,
            flx_run_main,
            flx_run_exe,
            flx_arun_lib,
            flx_arun_main,
            flx_arun_exe,
        ):
        super().__init__(ctx)

        self.flxg = flxg
        self.cxx = cxx
        self.flx_run_exe  = flx_run_exe
        self.flx_arun_exe = flx_arun_exe
        self.flx_run_lib  = flx_run_lib
        self.flx_arun_lib = flx_arun_lib
        self.flx_run_main = flx_run_main
        self.flx_arun_main = flx_arun_main

    @fbuild.db.cachemethod
    def _run_flxg(self, src:fbuild.db.SRC, *,
            includes=[],
            syntaxes=[],
            imports=[],
            flags=[],
            include_std=True,
            preparse=False,
            buildroot=None,
            **kwargs) -> fbuild.db.DST:
        buildroot = buildroot or self.ctx.buildroot

        src = Path(src)
        #src_buildroot = src.addroot(buildroot)

        print("Buildroot        = " + buildroot)
        print("Src to flxg      = " + src)
        if preparse:
            dst = buildroot /"cache"/"binary"+_getcwd()/src
            dst = dst.replaceext('.par')
        else:
            dst =buildroot /"cache"/"text"+_getcwd()/src
            dst = dst.replaceext('.cpp')

        print("Expected flg dst = " + dst)
        #if src != src_buildroot:
        #    src_buildroot.parent.makedirs()
        #    src.copy(src_buildroot)
        #    src = src_buildroot

        #dst.parent.makedirs()

        cmd = [self.flxg]

        if preparse:
            cmd.append('-c')

        includes = set(includes)
        includes.add(src.parent)
        includes.add(dst.parent)

        imports = list(imports)
        syntaxes = list(syntaxes)
        if include_std:
            imports.insert(0, 'plat/flx.flxh')               # Unix filename correct here
            syntaxes.insert(0, '@grammar/grammar.files')     # Unix filename correct here

        cmd.extend('-I' + i for i in sorted(includes) if Path.exists(i))
        cmd.extend('--syntax=' + i for i in syntaxes)
        cmd.extend('--import=' + i for i in imports)
        cmd.append('--output_dir=' + Path(buildroot)/"cache"/"text")
        cmd.append('--cache_dir=' + Path(buildroot)/"cache"/"binary")
        cmd.extend(flags)

        if include_std:
            cmd.append('std')

        if src.ext == '.flx':
            cmd.append(src.replaceext(''))
        else:
            cmd.append(src)

        self.ctx.execute(cmd, self.flxg.name, '%s -> %s' % (src, dst),
                color='yellow', **kwargs)

        return dst

    def preparse(self, *args, **kwargs):
        return self._run_flxg(*args, preparse=True, **kwargs)

    def compile(self, *args, **kwargs):
        return self._run_flxg(*args, **kwargs)

    def _link(self, linker, src, dst=None, *,
            includes=[],
            macros=[],
            cflags=[],
            libs=[],
            lflags=[],
            objects=[],
            buildroot=None):
        buildroot = buildroot or self.ctx.buildroot

        #print("_link: C++ compile src = " + src)

        if dst is None:
            dst = src.replaceext('')
        dst = Path(dst).addroot(buildroot)

        obj = self.cxx.compile(src,
            includes=includes,
            macros=macros,
            buildroot=buildroot,
            flags=cflags)

        thunk_obj = self.cxx.compile(src[:-4]+"_static_link_thunk.cpp",
            includes=includes,
            macros=macros,
            buildroot=buildroot,
            flags=cflags)


        return linker(dst, list(chain(objects, [obj,thunk_obj])),
            libs=libs,
            flags=lflags,
            buildroot=buildroot)

    def link_exe(self, *args, async=True, macros=[], objects=[], **kwargs):
        macros = macros + ['FLX_STATIC_LINK']
        objs = objects + [
          (self.flx_arun_lib if async else self.flx_run_lib) ] + [
          (self.flx_arun_main if async else self.flx_run_main)
          ]

        return self._link(self.cxx.link_exe, *args,
            macros=macros,
            objects=objs,
            **kwargs)

    def link_lib(self, *args, **kwargs):
        return self._link(self.cxx.link_lib, *args, **kwargs)

    # --------------------------------------------------------------------------

    def run_lib(self, src, *args, async=True, **kwargs):
        if async:
            cmd = [self.flx_arun_exe]
        else:
            cmd = [self.flx_run_exe]

        cmd.append(src)

        return self.ctx.execute(cmd, *args, **kwargs)

    # --------------------------------------------------------------------------

    @fbuild.db.cachemethod
    def _run_flx_pkgconfig(self, src:fbuild.db.SRC) -> fbuild.db.DSTS:
        """
        Run flx_pkgconfig to generate the include files, normally done by flx
        command line harness but we're probably building it here.
        """

        flx_pkgconfig = self.ctx.buildroot / 'host'/'bin'/'flx_pkgconfig'
        resh = src.replaceext('.resh')
        includes = src.replaceext('.includes')

        cmd = [
            flx_pkgconfig,
            '--path+=' + self.ctx.buildroot / 'host'/'config',
            '--field=includes',
            '@' + resh]

        stdout, stderr = self.ctx.execute(
            cmd,
            flx_pkgconfig,
            '%s -> %s %s' % (src, resh, includes),
            color='yellow',
            stdout_quieter=1)

        with open(includes, 'w') as f:
            for include in stdout.decode('utf-8','ignore').strip().split(' '):
                print('#include %s' % include, file=f)

        return resh, includes


    def _build_link(self, function, src, dst=None, *,
            async=True,
            includes=[],
            flags=[],
            cxx_includes=[],
            cxx_cflags=[],
            cxx_libs=[],
            cxx_lflags=[]):
        obj = self.compile(src, includes=includes, flags=flags)
        self._run_flx_pkgconfig(obj)

        return function(obj, dst,
            async=async,
            includes=cxx_includes,
            libs=cxx_libs,
            cflags=cxx_cflags,
            lflags=cxx_lflags,
        )

    def _build_flx_pkgconfig_link(self, function, src, dst=None, *,
            async=False,
            includes=[],
            flags=[],
            cxx_includes=[],
            cxx_cflags=[],
            cxx_libs=[],
            cxx_lflags=[]):
        #print("_build_flx_pkgconfig_link: src="+src)
        #print("_build_flx_pkgconfig_link: dst="+dst)
        obj = self.compile(src, includes=includes, flags=flags)

        return function(obj, dst,
            async=async,
            includes=cxx_includes,
            libs=cxx_libs,
            cflags=cxx_cflags,
            lflags=cxx_lflags,
            macros=["FLX_NO_INCLUDES"],
        )

    def build_lib(self, *args, **kwargs):
        return self._build_link(self.link_lib, *args, **kwargs)

    def build_exe(self, *args, **kwargs):
        return self._build_link(self.link_exe, *args, **kwargs)

    def build_flx_pkgconfig_exe(self, *args, **kwargs):
        return self._build_flx_pkgconfig_link(self.link_exe, *args, **kwargs)

# ------------------------------------------------------------------------------

def build(ctx, flxg, cxx, drivers):
    return Builder(
        ctx,
        flxg,
        cxx,
        drivers.flx_run_lib,
        drivers.flx_run_main,
        drivers.flx_run_exe,
        drivers.flx_arun_lib,
        drivers.flx_arun_main,
        drivers.flx_arun_exe,
    )

def build_flx_pkgconfig(host_phase, target_phase, flx_builder):
    print('[fbuild] [flx] building flx_pkgconfig')
    return flx_builder.build_flx_pkgconfig_exe(
        dst=Path('host')/'bin'/'flx_pkgconfig',
        src=target_phase.ctx.buildroot/'share'/'src'/'tools'/'flx_pkgconfig.flx',
        includes=[
          target_phase.ctx.buildroot / 'host'/'lib',
          target_phase.ctx.buildroot / 'share'/'lib',
          ],
        cxx_includes=[ 
                      target_phase.ctx.buildroot / 'share'/'lib'/'rtl', 
                      target_phase.ctx.buildroot / 'host'/'lib'/'rtl'],
        cxx_libs=[call('buildsystem.flx_rtl.build_runtime', host_phase, target_phase).static],
    )


def build_flx(host_phase, target_phase, flx_builder):
    print('[fbuild] [flx] building flx')
    return flx_builder.build_exe(
        async=False,
        dst=Path('host')/'bin'/'bootflx',
        src=target_phase.ctx.buildroot/'share'/'src'/'tools'/'bootflx.flx',
        includes=[
          target_phase.ctx.buildroot / 'host'/'lib',
          target_phase.ctx.buildroot / 'share'/'lib',
          ],
        cxx_includes=[ 
                      target_phase.ctx.buildroot / 'share'/'lib'/'rtl', 
                      target_phase.ctx.buildroot / 'host'/'lib'/'rtl'],
        cxx_libs=[
          call('buildsystem.flx_rtl.build_runtime', host_phase, target_phase).static,
          call('buildsystem.re2.build_runtime', host_phase).static,
          ],
    )


