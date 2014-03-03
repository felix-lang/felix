// SDL STUFF
#include "SDL2/SDL.h"
#include "SDL2/SDL_ttf.h"

// FELIX STUFF
#include "flx_world.hpp"
#include "flx_async_world.hpp"
#include "flx_async.hpp"
#include <string>

// ARGV/ARGC Setter callback
static int get_flx_args_config(int argc, char **argv, flx::run::flx_config *c) {
  c->static_link = true;
  c->filename = argv[0];
  c->flx_argv = argv;
  c->flx_argc = argc;
  c->debug = false;
  return 0;
}

// ASYNC subsystem setter callback
static void init_ptr_create_async_hooker(flx::run::flx_config *c, bool debug_driver) {
  c->ptr_create_async_hooker = create_async_hooker;
}

// Felix entry points
extern "C" void *felix_evtdemo_create_thread_frame();
extern "C" flx::rtl::con_t *felix_evtdemo_flx_start();

// library module constructor
static flx::rtl::flx_dynlink_t *link_library(flx::run::flx_config *c) {
  flx::rtl::flx_dynlink_t* library;
  library = new flx::rtl::flx_dynlink_t(
      string("felix_evtdemo"),
      (::flx::rtl::thread_frame_creator_t)felix_evtdemo_create_thread_frame,
      (::flx::rtl::start_t)felix_evtdemo_flx_start,
      0); //(flx::rtl::main_t)&flx_main);
  return library;
}

// MAINLINE
int main(int argc, char **argv)
{
  // create Felix world configuration from callbacks
  auto cfg = new flx::run::flx_config(
   link_library, 
   init_ptr_create_async_hooker, 
   get_flx_args_config
  );

  // create world from configuration
  auto world = new flx::run::flx_world(cfg);

  // setup the world with program arguments
  int error_exit_code = world->setup(argc, argv);

  // initialise the world
  world->begin_flx_code();

  printf("Felix world initialised\n");

  SDL_Init(SDL_INIT_VIDEO);
  TTF_Init();

  char const *font_file = "/Library/Fonts/Courier New Bold.ttf";
  auto font = TTF_OpenFont (font_file, 12);
  auto w = SDL_CreateWindow("Stuff", SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, 512,512,
   SDL_WINDOW_SHOWN | SDL_WINDOW_RESIZABLE);
  SDL_PumpEvents();
  SDL_Event e;

  printf("SDL world initialised, running SDL main loop\n");
  while (1) {
    // SDL Polling Loop
    while(SDL_PollEvent (&e))
    {
      printf("got an event\n");
      if (e.type == SDL_QUIT) goto exit_point;
      world->run_until_blocked();
    }
  }
exit_point:
  SDL_Quit();
  exit(0);
}
