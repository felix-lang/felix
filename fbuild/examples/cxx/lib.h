#ifndef LIB_H
#define LIB_H

#if defined(_WIN32) && !defined(STATIC_LINK)
#  if defined(BUILD_LIB)
#    define LIB_EXTERN __declspec(dllexport)
#  else
#    define LIB_EXTERN __declspec(dllimport)
#  endif
#else
#  define LIB_EXTERN
#endif

class LIB_EXTERN Fred
{
public:
    Fred(int i);

    int fred();

private:
    int i;
};

#endif
