module ParseLib(-- defined in ParseCore
                Pos,ParseError(..),ParseResult(..),ParseBad(..),ParseGood(..),Parser(..)
               --,initError,initBad,initGood      -- Start values for parseError,parseBad, parseGood
	       ,parseit
               ,parse,bind,ap,chk,orelse        -- The core
               ,token                           -- parse terminal
	       ,eof                             -- parse end of file
               ,parseFail                       -- Failing parser
                -- defined in ParseLib
               ,revAp                           -- Apply snd to fst
               ,revChk                          -- Check fst
               ,cases                           -- Muliple chk
               ,parseAp,parseChk                -- parse & (ap | chk)
               ,apCut,chkCut                    -- No return if fst succeed
               ,literal                         -- Parse literal
               ,many,some                       -- Zero/one or more items. Cut after each item.
               ,manySep,someSep                 -- Zero/one or more items with separator. Cut after each item.
               ) where

--import Prelude(error,(.),not,ord,chr,otherwise,(&&),(||),fromIntegral)

--import Prelude(show)

import ParseCore

import Utils(chr,ord)

infixl 5 `parseAp`
infixl 5 `revAp`
infixl 5 `apCut`
infixl 5 `parseChk`
infixl 5 `revChk`
infixl 5 `chkCut`

revAp :: Parser p a i c -> Parser p (a->b) i c -> Parser p b i c
revAp     x y = \good bad ->
                x       (\u -> y (\v -> good (v u)) bad)
                        bad

revChk :: Parser p a i c -> Parser p b i c -> Parser p b i c
revChk     x y = \good bad ->
                x       (\_  -> y good bad)
                        bad

{-
cases :: (Ord lex,Text lex) =>
       [(lex,Pos a -> Parser p  b  [(Pos a,lex)] c)]
       -> Parser p b  [(Pos a,lex)] c
       -> Parser p b  [(Pos a,lex)] c
-}
cases tps dp = \good bad input@((pos,t):input') err@(pe,et,msg) ->
        if pe > pos then
                cases' pos t good input' (dp good bad input err) tps
        else
                cases'' pos t good input' (dp good bad input) pos (show t) (if pos > pe then [] else msg)  tps
        where

        {-
	cases' :: Pos a -> lex -> ParseGood b [(Pos a,lex)] c
                                  -> [(Pos a,lex)]
                                  -> ParseResult c [(Pos a,lex)]
                                  -> [(lex,Pos a -> Parser p b  [(Pos a,lex)] c)]
                                  -> ParseResult c [(Pos a,lex)]
        -}
	cases' pos t good input' dp [] =
                dp
        cases' pos t good input' dp ((t',p):tps) =
                if t == t' then
                        p pos good initBad input' initError
                else
                        cases' pos t good input' dp tps

        {-
	cases'' :: Pos a -> lex -> ParseGood b [(Pos a,lex)] c
                                   -> [(Pos a,lex)]
                                   -> (ParseError -> ParseResult c [(Pos a,lex)])
                                   -> Pos a
                                   -> String
                                   -> [String]
                                   -> [(lex,Pos a -> Parser p b  [(Pos a,lex)] c)]
                                   -> ParseResult c [(Pos a,lex)]
        -}
	cases'' pos t good input' dp ep et em [] =
                dp (ep,et,em)
        cases'' pos t good input' dp ep et em ((t',p):tps) =
                if t == t' then
                        p pos good initBad input' initError
                else
                        cases'' pos t good input' dp ep et (show t' : em) tps


parseAp :: (a->b) -> Parser p a i c -> Parser p b i c
parseAp     x y = \good ->
                        y (\v -> good (x v) )

parseChk :: b -> Parser p a i c -> Parser p b i c
parseChk    x y = \good ->
                        y (\_  -> good x)


--apCut :: Parser (Pos a) (a->b) i c -> Parser (Pos a) a i c -> Parser (Pos a) b i c
apCut     x y = \good bad->
                x       (\u input' err' -> y (\v -> good (u v)) initBad input' initError)
                        bad

--chkCut :: Parser (Pos a) b i c -> Parser (Pos a) a i c -> Parser (Pos a) b i c
chkCut     x y = \good bad ->
                x       (\u input' err' -> y (\_ -> good u) initBad input' initError )
                        bad


---------  Next section doesn't care about the internal structure
literal t = token (\pos t' -> if t==t' then Right pos else Left (show t))

many p = some p `orelse` parse []

some p = (:) `parseAp` p `apCut` many p

manySep' s p = s `revChk` someSep s p
                 `orelse`
               parse []

manySep s p = someSep s p `orelse` parse []

someSep s p = (:) `parseAp` p `apCut` manySep' s p
