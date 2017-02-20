{-# COMPILERFLAGS -fno-overload-restr #-}
module ParsOps3 where
import LLParsOps
import Trace

infixl 5 `ap`,`apCut`,`chk`
infixr 4 `orelse`
--infix 3 `err`

parse p = parseDP p

parseToEof p is =
  case parseDP p is of
    (Just x,[],es) -> Right x -- warnings are discarded
    (_,_,es) -> trace (show es) $ Left (head [(is,fs)|(is,fs,Nothing)<-es])

ap x = seqDP x
apCut x= ap x

p1 `chk` p2 = const `mapP` p1 `ap` p2
p1 `cap` p2 = const id `mapP` p1 `ap` p2

orelse x = altDP x

unit x = emptyDP x
empty x = emptyDP x
--err = errDP
tok x = symbolDP x
oneOf x = anySymbolDP x

mapP f p = unit f `ap` p

many x = fst . manysome $ x
some x = snd . manysome $ x

manysome p = (many,some)
  where
    many = some `orelse` empty []
    some = (:) `mapP` p' `ap` many
    p' = nonEmptyDP "some or many" p

optional x p = p `orelse` empty x
maybeP p = Just `mapP` p `orelse` unit Nothing
