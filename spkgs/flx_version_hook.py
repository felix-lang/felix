caml_modules = [
    'src/compiler/flx_version_hook/flx_version_hook',
]

caml_include_paths = [
    'src/compiler/flx_version',
    'src/compiler/flx_version_hook',
]

caml_provide_lib = 'src/compiler/flx_version_hook/flx_version_hook'

pkg_requires = ['flx_version']
