module NntpF(nntpF,NntpRequest(..),NntpResponse(..),Part(..)) where
import Fudgets
import NntpIO

data NntpRequest = GetGroup String
                 | GetArticle [Part] String Int
		 | GetArticleWithId [Part] String
		 deriving (Show)

data NntpResponse = Group Group
                  | Article [Part] Int String [String] -- parts # id lines
                  | NntpError String
		  deriving (Show)

type Group = (String,Int,Int,Int) -- (name,cnt,first,last)

data Part = Head | Body deriving (Show)

nntpF = loopThroughRightF (absF nntpSP0) nntpIOF

nntpSP0 =
  getNntp $ \ msg ->
  nntpSP ""

nntpSP group =
  getCmd $ \cmd ->
  execNntpCmd group cmd $ \ group' resp ->
  putResp resp $
  nntpSP group'

putNntp cmd = putSP (Left cmd)
putResp resp = putSP (Right resp)

execNntpCmd group cmd cont =
  case cmd of
    GetGroup group' ->
      nntpCmd ("GROUP "++group') $
      cont group'
    GetArticle parts group' n ->
      (if group'==group
       then id
       else nntpCmd ("GROUP "++group') . const) $
      nntpCmd (unwords [partCmd parts,show n]) $
      cont group'
    GetArticleWithId parts id ->
      nntpCmd (unwords [partCmd parts,id]) $
      cont group

partCmd [] = "STAT"
partCmd [Head] = "HEAD"
partCmd [Body] = "BODY"
partCmd _ = "ARTICLE"

nntpCmd s cont =
  putNntp s $
  getNntp $ \ r ->
  cont (resp r)

resp (status:text) =
  case words status of
    "211":cnt:first:last:group:_ ->
      Group (group,read cnt,read first,read last)
    "220":n:id:_ ->
      Article [Head,Body] (read n) id text
    "221":n:id:_ ->
      Article [Head] (read n) id text
    "222":n:id:_ ->
      Article [Body] (read n) id text
    "223":n:id:_ ->
      Article [] (read n) id text
    _ -> NntpError status
