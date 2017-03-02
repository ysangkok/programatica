
#if 0
/* obsolete */
#define MAXALLOC 25000 /* must be large enough for 256 CharStructs */

extern void addnumresult PROTO ((int, Window, unsigned long));

#define addxidresult(ev,win,xid) addnumresult(ev,win,(unsigned long)xid)
#define addptrresult(ev,win,ptr) addnumresult(ev,win,(unsigned long)ptr)
#endif

#define mkPoint(x,y) mkpair(mkint(x),mkint(y))
#define mkRect(x1,y1,x2,y2) mkpair(mkPoint(x1,y1),mkPoint(x2,y2))
#define mkenum(n) mkconstr0(n)
#define mkAtom(a) mkint(a)
#define mkWindow(a) mkint(a)
#define mkRGB(r,g,b) mkconstr3(0,mkint(r),mkint(g),mkint(b))
#define mkColor(c) mkconstr2(0,mkint((c).pixel),mkRGB((c).red,(c).green,(c).blue))

extern PTR mknstring PROTO ((unsigned char *,unsigned int));
extern PTR mkStringList PROTO ((char **,unsigned int));

#ifdef _XLIB_H_
extern PTR mkFontInfoList PROTO ((char **,XFontStruct *,unsigned int));

/*extern PTR mkFontStruct PROTO ((XFontStruct *));*/
extern PTR mkFontStructListMaybe PROTO ((XFontStruct *));
extern PTR mkFontStructList PROTO ((XFontStruct *));
extern PTR mkCharStruct PROTO ((XCharStruct *));
extern PTR encodeEvent PROTO ((XEvent *));
extern PTR mkModState PROTO ((unsigned int modstate));

extern Time selreqtime; 
/* time from last SelectionRequest, used in SendEvent SelectionNotify */
#endif


