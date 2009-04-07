from fbuild.flxbuild.flxutil import mkdirs
iscr_source=['lpsrc/flx_doxygen.pak']

# dir must exist for doxygen to work
mkdirs(os.path.join('doc','rtl','doxygen'))
