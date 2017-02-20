{-# COMPILERFLAGS -fallow-redef #-}
module Alfa.ConvToAgda where
--import Prelude hiding (Floating) -- to hide exp
import qualified Alfa.ConvToAgdaInEnv as E

exp      = std E.exp
decl     = std E.decl
decls    = std E.decls
var      = std E.var
con      = std E.con
label    = std E.label
branch   = std E.branch
branches = std E.branches
context  = std E.context
context' = std E.context'
constr   = std E.constr

decls'   = strip E.decls

std   m x = E.runEnv (m x) False
strip m x = E.runEnv (m x) True  -- strips ND style proofs
