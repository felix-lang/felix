import fbuild.builders.sdl
import fbuild.builders.text
import fbuild.config.c.gsl
import fbuild.config.c.sdl
import fbuild.config.c.mpi
import fbuild.config.c.pari
from fbuild.path import Path

import buildsystem

# ------------------------------------------------------------------------------

def build_flx(phase):
    dsts = []

    if fbuild.config.c.gsl.gsl_gsl_blas_h(phase.cxx.shared).header:
        dsts.extend(buildsystem.copy_fpc_to_config(phase.ctx,
            Path('src/gsl/*.fpc').glob()))

        dsts.extend(buildsystem.copy_flxs_to_lib(phase.ctx,
            Path('src/gsl/*.flx').glob()))

    if 'macosx' in phase.platform:
        gl_fpc = fbuild.builders.text.autoconf_config_file(phase.ctx,
            'src/opengl/gl.fpc', 'src/opengl/gl.fpc.in', {
                'SHARED_LIBS': '-framework OpenGL',
                'STATIC_LIBS': '-framework OpenGL',
            })

        glu_fpc = fbuild.builders.text.autoconf_config_file(phase.ctx,
            'src/opengl/glu.fpc', 'src/opengl/glu.fpc.in', {
                'SHARED_LIBS': '-framework OpenGL',
                'STATIC_LIBS': '-framework OpenGL',
            })

        glext_fpc = fbuild.builders.text.autoconf_config_file(phase.ctx,
            'src/opengl/glext.fpc', 'src/opengl/glext.fpc.in', {
                'SHARED_LIBS': '-framework OpenGL',
                'STATIC_LIBS': '-framework OpenGL',
            })

        glut_fpc = fbuild.builders.text.autoconf_config_file(phase.ctx,
            'src/opengl/glut.fpc', 'src/glut/glut.fpc.in', {
                'SHARED_LIBS': '-framework GLUT',
                'STATIC_LIBS': '-framework GLUT',
            })
    else:
        gl_fpc = fbuild.builders.text.autoconf_config_file(phase.ctx,
            'src/opengl/gl.fpc', 'src/opengl/gl.fpc.in', {
                'SHARED_LIBS': '-lGL',
                'STATIC_LIBS': '-lGL',
            })

        glu_fpc = fbuild.builders.text.autoconf_config_file(phase.ctx,
            'src/opengl/glu.fpc', 'src/opengl/glu.fpc.in', {
                'SHARED_LIBS': '-lGLU',
                'STATIC_LIBS': '-lGLU',
            })

        glext_fpc = fbuild.builders.text.autoconf_config_file(phase.ctx,
            'src/opengl/glext.fpc', 'src/opengl/glext.fpc.in', {
                'SHARED_LIBS': '-lGLEXT',
                'STATIC_LIBS': '-lGLEXT',
            })

        glut_fpc = fbuild.builders.text.autoconf_config_file(phase.ctx,
            'src/opengl/glut.fpc', 'src/glut/glut.fpc.in', {
                'SHARED_LIBS': '-lglut',
                'STATIC_LIBS': '-lglut',
            })

    dsts.extend(buildsystem.copy_fpc_to_config(phase.ctx,
        [gl_fpc, glu_fpc, glext_fpc, glut_fpc]))

    dsts.extend(buildsystem.copy_to(phase.ctx,
        phase.ctx.buildroot / 'lib/GL', Path('src/opengl/*.flx').glob()))

    if fbuild.config.c.sdl.SDL_SDL_h(phase.cxx.shared).header:
        sdl_config = fbuild.builders.sdl.SDLConfig(phase.ctx)
        sdl_fpc = fbuild.builders.text.autoconf_config_file(phase.ctx,
            'src/sdl/sdl.fpc', 'src/sdl/sdl.fpc.in', {
                'VERSION': sdl_config.version(),
                'CFLAGS': sdl_config.cflags(),
                'SHARED_LIBS': sdl_config.libs(),
                'STATIC_LIBS': sdl_config.static_libs(),
            })

        dsts.extend(buildsystem.copy_fpc_to_config(phase.ctx, [sdl_fpc]))

        dsts.extend(buildsystem.copy_to(phase.ctx,
            phase.ctx.buildroot / 'lib/SDL', Path('src/sdl/*.flx').glob()))

    if fbuild.config.c.mpi.mpi_h(phase.cxx.shared).header:
        dsts.extend(buildsystem.copy_fpc_to_config(phase.ctx,
            Path('src/mpi/*.fpc').glob()))

        dsts.extend(buildsystem.copy_flxs_to_lib(phase.ctx,
            Path('src/mpi/*.flx').glob()))

    if fbuild.config.c.pari.pari_h(phase.cxx.shared).header:
        dsts.extend(buildsystem.copy_fpc_to_config(phase.ctx,
            Path('src/pari/*.fpc').glob()))

        dsts.extend(buildsystem.copy_flxs_to_lib(phase.ctx,
            Path('src/pari/*.flx').glob()))

    return dsts
