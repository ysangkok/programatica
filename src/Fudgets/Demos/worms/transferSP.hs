module transferSP where
import Output
import IOlml
import Termcap
import Fudgets
import NonStdTrace
import utils(chr,ord)

#include "encoding.h"

encode1 cmd =
      case cmd
      of Str' {-'-} s-> ((STRTAG): s) ++ [(STREND)]
         Flush-> FLUSH:doFlush
         Moveto x y-> MOVETAG:chr (x+1):chr (y+1):[]
         Clr-> [CLEAR]

decodeSP =
    getSP (\c->
      case c
      of STRTAG -> splitAtElemSP (== STREND ) (\s->putSP s decodeSP)
         FLUSH -> putSP doFlush decodeSP
         MOVETAG -> getSP (\x->getSP (\y->putSP (moveTo (ord x-1) (ord y-1)) decodeSP))
         CLEAR -> putSP clear decodeSP
         _->trace ("Got "++[c]) decodeSP -- ??
           )
