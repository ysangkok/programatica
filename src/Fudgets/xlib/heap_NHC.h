/* Heap access functions for NHC */

#include "cinterface.h"
#include "node.h"
#include "mk.h"
#include "twopass.h"
#include "proto.h"

#ifdef Socket   /* Needed since Socket is a type in nhc:s prelude */
#undef Socket
#endif

typedef NodePtr PTR;
/* Invariant: PTRs passed as function arguments or results NEVER
 *            point to indirection nodes!
 */

#define DOGC        C_GC(1000)
#define PUSHPTR(p)  C_PUSH(p)
#define POPPTR(p)   (p) = C_POP()

#define SAVE(p,c) {PUSHPTR(p); c; POPPTR(p); }

#define GCSAVE(p) do { PUSHPTR(p); C_CHECK(1000); POPPTR(p); } while(0)
#define BUFFER 100 /* used by C_CHECK_* */

#define EVALUATE_FROM_C 1

#if EVALUATE_FROM_C
extern PTR evaluate(PTR p);
#else
/* Evaluation is NOT done from C code. */
#define evaluate(p) p
#endif

extern PTR getpart(PTR p,int pos,int argcnt);
extern PTR mkconstr(/*int cno,int argcnt,...*/);
extern PTR mkconstr0(int cno);
extern PTR mkconstr1(int cno,PTR arg);

#define mkconstr3(cno,arg1,arg2,arg3) mkconstr(cno,3,arg1,arg2,arg3)
#define mkconstr2(cno,arg1,arg2) mkconstr(cno,2,arg1,arg2)

extern void evalstring(PTR p,char *buf,int bufsize);
extern void getPackedStringL(PTR p,char **buf,unsigned *len);

#define getcno(p) GET_CONSTR(p)

#define CHAROF(p) GET_INT_VALUE(p)
#define INTOF(p) GET_INT_VALUE(p)

#define FSTOF(p) getpart(p,0,2)
#define SNDOF(p) getpart(p,1,2)

#define ISNIL(p) (GET_CONSTR(p)==0)
#define HDOF(p) FSTOF(p)
#define TLOF(p) SNDOF(p)

#define GET_ENUM(p) GET_CONSTR(p)
#define GET_BOOL_VAL(p) GET_BOOL_VALUE(p)

#define mkint(i) mkInt(i)
extern PTR mkchar(unsigned char c);
#define mkpair(fst,snd) mkTuple2(fst,snd)
#define mkcons(hd,tl) mkCons(hd,tl)
#define mknil() mkNil()

#define mkNone() mkNothing()
#define mkSome(p) mkJust(p)

#define debug xlib_debug
extern int debug;

extern void mkresponse(PTR resp);
extern void mksuccess(void);

extern int outstring(FILE *,PTR p);

/* extern void fileerror(int ioerror,char *filename,char *oper); */

extern PTR mkestring(unsigned char *as);

#ifdef PROFILE

#define WHEN_NOT_REPLAY(x) if(!replay) x
#define WHEN_NOT_REPLAY_START if(!replay) {
#define WHEN_NOT_REPLAY_END   }

#define REPLAY_ONE_AND_SKIP(x) if(replay) { REPLAY(x); } else
#define RECORD_ONE(x) if(record) { RECORD(x);}

#else

#define WHEN_NOT_REPLAY(x)    x
#define WHEN_NOT_REPLAY_START 
#define WHEN_NOT_REPLAY_END   

#define REPLAY_ONE_AND_SKIP(x) 
#define RECORD_ONE(s) 

#endif
