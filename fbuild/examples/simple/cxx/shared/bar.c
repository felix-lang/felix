#if defined _WIN32 || defined __CYGWIN__
__declspec(dllexport)
#endif
const char* bar() {
	return "world";
}
