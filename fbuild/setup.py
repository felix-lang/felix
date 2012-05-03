import sys
from distutils.core import setup

sys.path.append('lib')
import fbuild

setup(
    name='fbuild',
    version=fbuild.__version__,
    description='fbuild build system',
    author='Erick Tryzelaar',
    author_email='erickt@felix-lang.org',
    url='https://github.com/felix-lang/fbuild',
    license='BSD',
    packages=[
        'fbuild',
        'fbuild.builders',
        'fbuild.builders.c',
        'fbuild.builders.c.clang',
        'fbuild.builders.c.gcc',
        'fbuild.builders.cxx',
        'fbuild.builders.cxx.clangxx',
        'fbuild.builders.cxx.gxx',
        'fbuild.builders.ocaml',
        'fbuild.config',
        'fbuild.config.c',
        'fbuild.config.cxx',
        'fbuild.db',
        'fbuild.subprocess',
    ],
    scripts=['bin/fbuild'],
    package_dir={'': 'lib'},
)
