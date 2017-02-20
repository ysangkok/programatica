-- GHC 4.03 bug workaround (lexical analyser fails on LA.!)
module LA(Array,sub,array,listArray) where
import Array

sub a i = a!i
