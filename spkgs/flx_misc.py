caml_modules = [
    'src/compiler/flx_misc/flx_filesys',
    'src/compiler/flx_misc/flx_list',
    'src/compiler/flx_misc/flx_set',
    'src/compiler/flx_misc/flx_string',
    'src/compiler/flx_misc/flx_util',
    'src/compiler/flx_misc/flx_getopt',
]

caml_require_libs = ["str"]

caml_provide_lib = 'src/compiler/flx_misc/flx_misc'
