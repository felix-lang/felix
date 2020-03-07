extern "C" void flx_pkgconfig_create_thread_frame();
extern "C" void flx_pkgconfig_flx_start();
void* static_create_thread_frame = (void*)flx_pkgconfig_create_thread_frame;
void* static_flx_start = (void*)flx_pkgconfig_flx_start;
