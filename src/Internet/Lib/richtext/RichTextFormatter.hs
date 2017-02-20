module RichTextFormatter where
import RichText
import Utils2(words')
import Data.List((\\))

type FormattedRichText a = RichTextPage	a -- [RichTextPage a]
type RichTextPage a = [RichTextPara a]
type RichTextPara a = ([ParaFmt],[RichTextWords a])
type RichTextWords a = ([CharFmt],[Word a])

data Word a = PlainWord String | SpecialWord a

formatRichText rt specials =
    reverse paras
  where
    (_,_,_,paras) = fmt [] [] [] [] specials rt

    fmt :: [CharFmt]->[ParaFmt]->[RichTextPara a]->[RichTextWords a]-> [a] -> RichText b -> ([CharFmt],[ParaFmt],[a],[RichTextPara a])
    fmt cfs pfs ps ws ss [] = (cfs,pfs,ss,(pfs,ws):ps)
    fmt cfs pfs ps ws ss (rt:rts) =
      case rt of
	PlainChars s ->
	  let ws' = (cfs,map PlainWord (words' s))
	  in fmt cfs pfs ps (ws++[ws']) ss rts
	Special _ ->
	  case ss of
	    s:ss' -> fmt cfs pfs ps (ws++[(cfs,[SpecialWord s])]) ss' rts
	    _ -> error "too few special values in formatRichText"
	FmtChar On  cf -> fmt (cf:cfs) pfs ps ws ss rts
	FmtChar Off cf -> fmt (cfs \\ [cf]) pfs ps ws ss rts
	FmtPara Comment' _ -> fmt cfs pfs ps ws ss rts
	FmtPara pf rtp ->
	  let (cfs',pfs',ss',ps') = fmt cfs (pf:pfs) ps ws ss rtp
	  in fmt cfs' pfs ps' [] ss' rts
	NewLine -> fmt cfs pfs ((pfs,ws):ps) [] ss rts
	NewPage -> fmt cfs pfs (([],[]):(pfs,ws):ps) [] ss rts

    {-
    (_,_,_,_,paras) = fmt 0 [] [] [] [] specials rt

    fmt :: Int->[CharFmt]->[ParaFmt]->[RichTextPara a]->[RichTextWords a]-> [a] -> RichText b -> (Int,[CharFmt],[ParaFmt],[a],[RichTextPara a])
    fmt n cfs pfs ps ws ss [] = (n,cfs,pfs,ss,(pfs,ws):ps)
    fmt n cfs pfs ps ws ss (rt:rts) =
      case rt of
	PlainChars s ->
	  let ws' = (cfs,map PlainWord (words' s))
	  in fmt n cfs pfs ps (ws++[ws']) ss rts
	Special _ ->
	  case ss of
	    s:ss' -> fmt (n+1) cfs pfs ps (ws++[(cfs,[SpecialWord s])]) ss' rts
	    _ -> error "too few special values in formatRichText"
	FmtChar On  cf -> fmt n (cf:cfs) pfs ps ws ss rts
	FmtChar Off cf -> fmt n (cfs \\ [cf]) pfs ps ws ss rts
	FmtPara Comment _ -> fmt n cfs pfs ps ws ss rts
	FmtPara pf rtp ->
	  let (n',cfs',pfs',ss',ps') = fmt n cfs (pf:pfs) ps ws ss rtp
	  in fmt n' cfs' pfs ps' [] ss' rts
	NewLine -> fmt n cfs pfs ((pfs,ws):ps) [] ss rts
	NewPage -> fmt n cfs pfs (([],[]):(pfs,ws):ps) [] ss rts
    -}
