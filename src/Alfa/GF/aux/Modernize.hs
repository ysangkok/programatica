module Main where

import System



-- modernize GF notation in a grammar file

main :: IO ()
main = do
  file:_ <- getArgs
  modernize file

modernize :: FilePath -> IO ()
modernize file = do
  s <- readFile file
  let s' = modr s
  let file' = if last file == 'n' then init file else file -- gfn > gf
  putStrLn file'
  putStrLn s'
  writeFile file' s'

modr s = case s of
  '"':s2 -> '"':pass s2 -- don't modernize in quotes!
  ':':':':s2 -> ':':modr s2
  'L':'i':'T':s2 -> 'L':'i':'n':modr s2
  'l':'n':'p':s2 -> 'p':'a':'t':'t':'e':'r':'n':modr s2
  'c':'a':'s':'e':'s':s2 -> 't':'a':'b':'l':'e':modr s2
  't':'b':'l':s2 -> 't':'a':'b':'l':'e':modr s2
  'i':'m':'p':'o':'r':'t':' ':s2 -> 'i':'n':'c':'l':'u':'d':'e':' ': modr s2
  '.':'g':'f':'n':' ':s2 -> '.':'g':'f':' ':modr s2
  c:s2 -> c:modr s2
  _ -> s
 where 
   pass t = case t of
     '"':s2 -> '"':modr s2
     c:s2 -> c:pass s2
     _ -> t
