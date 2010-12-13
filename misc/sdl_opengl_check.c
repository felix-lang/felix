// for testing whether we have SDL_opengl
#include <SDL.h>            // overrides main
#include <SDL_opengl.h>

int
main(int argc, char** argv)
{
    if(SDL_Init(SDL_INIT_VIDEO) == -1
        || SDL_SetVideoMode(640, 480, 32, SDL_ANYFORMAT | SDL_OPENGL) == NULL)
    {
        return 1;       // fail
    }

    // random gl stuff to make sure we link
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();

    return 0;           // pass
}
