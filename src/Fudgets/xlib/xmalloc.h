#ifdef NHC

#define xmalloc(n) malloc(n)
extern void *malloc(unsigned);

#else

/*
extern void *xmalloc(unsigned);
*/

#endif
