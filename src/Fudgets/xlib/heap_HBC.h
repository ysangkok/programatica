#include "../runtime/node.h"
#include "../runtime/funs.h"
#include "proto.h"

extern VPTR ehp, hp;
extern int debug;

#define SAVE(p,c) {PUSHPTR(p); c; POPPTR(p); }

#define GCSAVE(p) if (NEEDGC) { PUSHPTR(p); DOGC; POPPTR(p); }

extern void getPackedStringL PROTO ((PTR Ptr,char **s,unsigned *n));

#define mkNone() mkconstr(0,0)
#define mkSome(p) mkconstr(1,1,p)
#define mkBool(b) mkint(b)

#define mkconstr3(cno,arg1,arg2,arg3) mkconstr(cno,3,arg1,arg2,arg3)
#define mkconstr2(cno,arg1,arg2) mkconstr(cno,2,arg1,arg2)
#define mkconstr1(cno,arg) mkconstr(cno,1,arg)
#define mkconstr0(cno) mkconstr(cno,0)

#define GET_ENUM(p) INTOF(p)
#define GET_BOOL_VAL(p) INTOF(p)



#define WHEN_NOT_REPLAY(x)    x
#define WHEN_NOT_REPLAY_START 
#define WHEN_NOT_REPLAY_END   

#define REPLAY_ONE_AND_SKIP(x) 
#define RECORD_ONE(s) 
