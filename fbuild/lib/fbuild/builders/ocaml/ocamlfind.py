from functools import partial
from itertools import chain

import fbuild.builders
import fbuild.builders.ocaml as ocaml
import fbuild.db

# ------------------------------------------------------------------------------

class Ocamldep(ocaml.Ocamldep):
    """Overload ocaml.Ocamldep builder to use ocamlfind's ocamldep."""

    def __init__(self, ctx, exe=None, *args,
            ocamlfind_cmd='ocamldep',
            pre_flags=[],
            packages=[],
            syntaxes=[],
            ppopts=[],
            **kwargs):
        self.ocamlfind_cmd = ocamlfind_cmd
        self.packages = packages
        self.syntaxes = syntaxes
        self.ppopts = ppopts

        # We'll use ocamlfind as our executable and add ocamldep as the first
        # preflag.
        exe = fbuild.builders.find_program(ctx, [exe if exe else 'ocamlfind'])
        super().__init__(ctx, exe, *args,
            pre_flags=[ocamlfind_cmd] + pre_flags,
            **kwargs)

    def modules(self, *args,
            flags=[],
            packages=[],
            syntaxes=[],
            ppopts=[],
            **kwargs):
        """Calculate the module this ocaml file depends on."""

        # Add the ocamlfind-specific flags to the flags
        flags = list(flags)

        for package in chain(self.packages, packages):
            flags.extend(('-package', package))

        for syntax in chain(self.syntaxes, syntaxes):
            flags.extend(('-syntax', syntax))

        for ppopt in chain(self.ppopts, ppopts):
            flags.extend(('-ppopt', ppopt))

        return super().modules(*args, flags=flags, **kwargs)

    def __str__(self):
        return '%s %s' % (self.exe.name, self.ocamlfind_cmd)

# ------------------------------------------------------------------------------

class OcamlfindBuilderMixin:
    def where(self):
        stdout, stderr = self.ctx.execute(
            [self.exe, self.ocamlfind_cmd, '-where'],
            quieter=1)
        return Path(stdout.decode().strip())

    def version(self):
        """Return the version of the ocaml executable."""
        stdout, stderr = self.ctx.execute(
            [self.exe, self.ocamlfind_cmd, '-version'],
            quieter=1)
        return stdout.decode().strip()

    def _run(self, *args,
            flags=[],
            packages=[],
            syntaxes=[],
            ppopts=[],
            **kwargs):
        # Add the ocamlfind-specific flags to the flags
        flags = list(flags)

        for package in chain(self.packages, packages):
            flags.extend(('-package', package))

        for syntax in chain(self.syntaxes, syntaxes):
            flags.extend(('-syntax', syntax))

        for ppopt in chain(self.ppopts, ppopts):
            flags.extend(('-ppopt', ppopt))

        return super()._run(*args, flags=flags, **kwargs)

    def build_objects(self, *args,
            packages=[],
            syntaxes=[],
            ppopts=[],
            ocamldep_flags=[],
            **kwargs):

        # Add the ocamlfind-specific flags to the flags
        ocamldep_flags = list(ocamldep_flags)

        for package in chain(self.packages, packages):
            ocamldep_flags.extend(('-package', package))

        for syntax in chain(self.syntaxes, syntaxes):
            ocamldep_flags.extend(('-syntax', syntax))

        for ppopt in chain(self.ppopts, ppopts):
            ocamldep_flags.extend(('-ppopt', ppopt))

        return super().build_objects(*args,
            ocamldep_flags=ocamldep_flags,
            packages=packages,
            syntaxes=syntaxes,
            ppopts=ppopts,
            **kwargs)

    def link_exe(self, *args, flags=[], linkpkg=None, **kwargs):
        """Compile all the L{srcs} and link into an executable."""

        # Add the ocamlfind-specific link flags to the flags
        flags = list(flags)

        linkpkg = self.linkpkg if linkpkg is None else linkpkg
        if linkpkg:
            flags.append('-linkpkg')

        return super().link_exe(*args, flags=flags, **kwargs)

    def __str__(self):
        return '%s %s' % (self.exe.name, self.ocamlfind_cmd)

# ------------------------------------------------------------------------------

class Ocamlc(OcamlfindBuilderMixin, ocaml.Ocamlc):
    """Overload ocaml.Ocamlc builder to use ocamlfind's ocamlc."""

    def __init__(self, ctx, exe=None, *args,
            ocamlfind_cmd='ocamlc',
            pre_flags=[],
            packages=[],
            syntaxes=[],
            linkpkg=True,
            ppopts=[],
            make_ocamldep=Ocamldep,
            **kwargs):
        self.ocamlfind_cmd = ocamlfind_cmd
        self.packages = packages
        self.syntaxes = syntaxes
        self.linkpkg = linkpkg
        self.ppopts = ppopts

        # We'll use ocamlfind as our executable and add ocamlc as the first
        # preflag.
        exe = fbuild.builders.find_program(ctx, [exe if exe else 'ocamlfind'])
        super().__init__(ctx, exe, *args,
            pre_flags=[ocamlfind_cmd] + pre_flags,
            make_ocamldep=partial(make_ocamldep,
                packages=packages,
                syntaxes=syntaxes,
                ppopts=ppopts),
            **kwargs)

# ------------------------------------------------------------------------------

class Ocamlcp(OcamlfindBuilderMixin, ocaml.Ocamlcp):
    """Overload ocaml.ocamlfind.Ocamlc to use ocamlfind's ocamlcp."""

    def __init__(self, ctx, exe=None, *args,
            ocamlfind_cmd='ocamlcp',
            pre_flags=[],
            packages=[],
            syntaxes=[],
            linkpkg=True,
            ppopts=[],
            make_ocamldep=Ocamldep,
            **kwargs):
        self.ocamlfind_cmd = ocamlfind_cmd
        self.packages = packages
        self.syntaxes = syntaxes
        self.linkpkg = linkpkg
        self.ppopts = ppopts

        # We'll use ocamlfind as our executable and add ocamlc as the first
        # preflag.
        exe = fbuild.builders.find_program(ctx, [exe if exe else 'ocamlfind'])
        super().__init__(ctx, exe, *args,
            pre_flags=[ocamlfind_cmd] + pre_flags,
            make_ocamldep=partial(make_ocamldep,
                packages=packages,
                syntaxes=syntaxes,
                ppopts=ppopts),
            **kwargs)

# ------------------------------------------------------------------------------

class Ocamlopt(OcamlfindBuilderMixin, ocaml.Ocamlopt):
    """Overload ocaml.Ocamlopt builder to use ocamlfind's ocamlopt."""

    def __init__(self, ctx, exe=None, *args,
            ocamlfind_cmd='ocamlopt',
            pre_flags=[],
            packages=[],
            syntaxes=[],
            linkpkg=True,
            ppopts=[],
            make_ocamldep=Ocamldep,
            make_ocamlc=Ocamlc,
            **kwargs):
        self.ocamlfind_cmd = ocamlfind_cmd
        self.packages = packages
        self.syntaxes = syntaxes
        self.linkpkg = linkpkg
        self.ppopts = ppopts

        # We'll use ocamlfind as our executable and add ocamlopt as the first
        # preflag.
        exe = fbuild.builders.find_program(ctx, [exe if exe else 'ocamlfind'])

        super().__init__(ctx, exe, *args,
            pre_flags=[ocamlfind_cmd] + pre_flags,
            make_ocamldep=partial(make_ocamldep,
                packages=packages,
                syntaxes=syntaxes,
                ppopts=ppopts),
            make_ocamlc=partial(make_ocamlc,
                packages=packages,
                syntaxes=syntaxes,
                linkpkg=linkpkg,
                ppopts=ppopts),
            **kwargs)

# ------------------------------------------------------------------------------

class Ocaml(ocaml.Ocaml):
    """Overload ocaml.Ocaml builder to use ocamlfind."""

    def __init__(self, *args,
            make_ocamldep=Ocamldep,
            make_ocamlc=Ocamlc,
            make_ocamlcp=Ocamlcp,
            make_ocamlopt=Ocamlopt,
            packages=[],
            syntaxes=[],
            ppopts=[],
            **kwargs):
        super().__init__(*args,
            make_ocamldep=partial(make_ocamldep,
                packages=packages,
                syntaxes=syntaxes,
                ppopts=ppopts),
            make_ocamlc=partial(make_ocamlc,
                packages=packages,
                syntaxes=syntaxes,
                ppopts=ppopts),
            make_ocamlcp=partial(make_ocamlcp,
                packages=packages,
                syntaxes=syntaxes,
                ppopts=ppopts),
            make_ocamlopt=partial(make_ocamlopt,
                packages=packages,
                syntaxes=syntaxes,
                ppopts=ppopts),
            **kwargs)
