{-# LANGUAGE CPP #-}
module CSizes where

#define CSIZE(ctype) fudsizeof_/**/ctype :: Int; fudsizeof_/**/ctype = error "fudsizeof_/**/ctype called"

#include "csizes.h"
