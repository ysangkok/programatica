extern PTR evalArgs PROTO((PTR, int));

#define GET1OF1(p) getpart(p, 0, 1)
#define GET1OF2(p) getpart(p, 0, 2)
#define GET2OF2(p) getpart(p, 1, 2)
#define GET1OF3(p) getpart(p, 0, 3)
#define GET2OF3(p) getpart(p, 1, 3)
#define GET3OF3(p) getpart(p, 2, 3)

#define ARG1(p) getpart(p, 0, 1)
#define ARG2(p,n) getpart(p, n-1, 2)
#define ARG3(p,n) getpart(p, n-1, 3)
#define ARG4(p,n) getpart(p, n-1, 4)

#define ATOMOF(p) INTOF(p)

#define SAVE(p,c) {PUSHPTR(p); c; POPPTR(p); }
#define EARG1(p) evaluate(ARG1(p))
#define ETLOF(p) evaluate(TLOF(p))
#define EHDOF(p) evaluate(HDOF(p))

