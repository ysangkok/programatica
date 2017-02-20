module Cache(allcacheF, bitmapdatacacheF, bitmapfilecacheF, fontcursorcacheF,
             colorcacheF, gCcacheF, fstructcacheF, fontcacheF) where
import Command
import Event
import FRequest
--import Font(FontStruct)
import Fudget
--import Geometry(Line, Point, Rect, Size(..))
--import LayoutRequest(LayoutRequest)
import Loopthrough
--import Message(Message(..))
--import Path(Path(..))
--import SP
--import Sockets
import Spops
import Xtypes(Pixel(..),colorPixel)
import HbcWord() -- Eq Word??
import LoopLow
import Cont
import IsRequest
--import Defaults
import DialogueIO hiding (IOError)
--import Maptrace(ctrace) -- debug

import NonStdTrace(trace)

data Cachecmd c r = CacheCmd c | NoCacheCmd | FreeCmd r

fontcacheF = cacheF $ \cmd -> case cmd of
           XReq (LoadFont fn) -> CacheCmd fn
	   _ -> NoCacheCmd

fstructcacheF = cacheF $ \cmd -> case cmd of
          XReq (QueryFont f) -> CacheCmd (Left f)
          XReq (LoadQueryFont s) -> CacheCmd (Right s)
	  _ -> NoCacheCmd

fontlistcacheF = cacheF $ \cmd ->
   case cmd of
     XReq (ListFonts pattern maxnames) -> CacheCmd (pattern,maxnames)
     XReq (ListFontsWithInfo pattern maxnames) -> CacheCmd (pattern,maxnames)
     _ -> NoCacheCmd

gCcacheF = cacheF $ \cmd -> case cmd of
	  XReq (CreateGC d t al) -> CacheCmd (d,t,al)
	  XCmd (FreeGC id) -> FreeCmd $ \ _ (GCCreated id') -> id == id'
	  _ -> NoCacheCmd

colorcacheF = cacheF $ \cmd -> case cmd of
	  XReq (AllocNamedColor cm cn) -> CacheCmd (cm,Left cn)
	  XReq (AllocColor cm rgb) -> CacheCmd (cm,Right rgb)
	  XCmd (FreeColors cm [pixel] (Pixel 0)) -> -- can only handle one pixel!
	    FreeCmd $ \(cm',_) e -> case e of
	       ColorAllocated (Just clr) ->
	         cm == cm' && colorPixel clr == pixel
               _ -> False
	  _ -> NoCacheCmd

fontcursorcacheF = cacheF $ \cmd -> case cmd of
	  XReq (CreateFontCursor shape) -> CacheCmd shape
	  _ -> NoCacheCmd

bitmapfilecacheF = cacheF $ \cmd -> case cmd of
	  XReq (ReadBitmapFile name) -> CacheCmd name
	  _ -> NoCacheCmd

bitmapdatacacheF = cacheF $ \cmd -> case cmd of
	  XReq (CreateBitmapFromData name) -> CacheCmd name
	  _ -> NoCacheCmd

allcacheF =
    fontcacheF .
    fstructcacheF .
    fontlistcacheF .
    gCcacheF .
    colorcacheF .
    --bitmapdatacacheF .
    bitmapfilecacheF .
    fontcursorcacheF

cacheF isR f = loopThroughLowF (cc []) f where
  cc table = same where
     same = getSP cachehandle
     cachehandle msg = case msg of 
       Left tc@(tag,c) -> case isR c of
	     CacheCmd d -> case tlook (\d' e->(d'==d)) table of
	        Just (_,r,n,tabler) -> putSP (Right (tag,r)) $
					  nseq (n+1) $ \n' ->
					  cc ((d,r,n'):tabler)
		Nothing -> waitresp $ \r ->
		           --ctrace "trcache" ("alloc",c,d,r) $
		           cc ((d,r,1):table)
	     FreeCmd p ->
	       --ctrace "trcache" ("free",c) $
	       case tlook p' table of
	        Just (d,r,n,tabler) -> if n == 1 then pass $ cc tabler 
				       else nseq (n-1) $ \n' ->
				            cc ((d,r,n'):tabler) 
	        Nothing -> trace ("cacheF: Free without request. "{-++show c-})
			   psame
               where p' d (XResp r) = p d r
		     p' _ _ = False
	     NoCacheCmd -> if isRequest c then waitresp $ \r -> same
			   else psame
         where waitresp c = cmdContSP (Left tc)
		 (\msg->case msg of Right te@(_,e) | isResponse e -> Just te
				    _ -> Nothing) $ \tr@(_,r) ->
		 putSP (Right tr) $ c r
       Right _ -> psame
      where
       pass = putSP msg
       psame = pass same


nseq :: Int -> (Int -> c) -> c
nseq n c = if n == n then c n else error "nseq"

tlook p = tl [] where
  tl t [] = Nothing
  tl t ((d,e,n):t') | p d e = seqlist tr $ Just (d,e,n,tr)
     where tr = t ++ t'
  tl t (x:t') = tl (x:t) t'

seqlist l c = case l of [] -> c; _:l -> seqlist l c
