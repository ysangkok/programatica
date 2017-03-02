#ifndef PROTO

#if defined(__ANSI__) || defined(__GNUC__)
#define PROTO(x) x
#else
#define PROTO(x) ()
#endif

#endif
