
{- Bad abstractions and ditto names: -}

#define SETI(ctype,p,i,field,v) (_casm_ ``((ctype *)%0)[%1].field=%2;'' ((p)::HT(ctype)) (i) (v) :: IO ())

#define SINDEX(ctype,p,i,v) (_casm_ ``((ctype *)%0)[%1]=%2;'' (p) (i) (v) :: IO ())

#define SET(ctype,p,field,v) \
 (_casm_ ``((ctype *)%0)->field=%1;'' (addrOf (p)) (v) :: IO ())

#define MSET(ctype,p,field,v) \
  do cv <- marshallM (v) ; SET(ctype,p,field,cv)
     
#define GET(ctype,p,field) \
 (_casm_ `` %r =   ((ctype *)%0)->field ; '' (addrOf (p)))

#define AGET(ctype,p,field) \
 (_casm_ `` %r = &(((ctype *)%0)->field); '' ((p)::HT(ctype)))

#define INDEX(ctype) (\pP iI -> _casm_ ``%r = (ctype *)%0 + (int)%1;'' ((pP) :: HT(ctype)) iI)

#define CINDEX(t) (\pP iI -> _casm_ ``%r = ((t *)%0)[(int)%1];'' pP iI)

#define SETWa(swa,field,v) SET(XSetWindowAttributes,swa,field,v)
