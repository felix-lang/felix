#include <stdio.h>

#if defined _WIN32 || defined __CYGWIN__
__declspec(dllimport)
#endif
const char* bar();

int main(int argc, char** argv) {
	printf("hello %s\n", bar());
	return 0;
}
