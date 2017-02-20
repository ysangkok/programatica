module Main where
import ParsOps
import Pos

main = interact (show . parse p)

p = many (upper `orelse` lower)

upper = toUpper `mapP` token
lower = toLower `mapP` token
