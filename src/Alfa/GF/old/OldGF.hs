module OldGF where

import Operations

-- AR 5/12/1999

-- Theory

data Term = Cons Fun
          | Var  Symb
          | Meta MetaSymb
          | App Term Term
          | Abs Symb Term
          | Ground Cat [Term]
          | Prod Symb Term Term
          | Predef (Ident,Ident)
             deriving (Eq,Read,Show)

data Fun  = Fun  Ident deriving (Eq,Read,Show)
data Symb = Symb Ident deriving (Eq,Read,Show)
data Cat  = Cat  Ident deriving (Eq,Read,Show)

type MetaSymb = (Cat,Ident)

type Ident = String
type OIDent = Ident

type Type         = Term
type Context      = [(Symb,Type)]
type Substitution = [(Symb,Term)]

data Patt = APatt Symb
          | FPatt Fun [Patt]
          | CPatt Cat [Patt] deriving (Eq, Read,Show)

data Definition = 
   DfRec  Patt Term       -- a = b 
 | DfData Patt [Ident]    -- A = {c,...,d}
 deriving (Eq, Read,Show)
-- a useful form of presentation for terms in beta normal form

termForm :: Term -> ([Symb],Term,[Term])
termForm t =
 case t of
  App c a -> ([],   c', a'++[a]) where (_,c',a')   = termForm c
  Abs x b -> (x:x', b', b'')     where (x',b',b'') = termForm b
  _       -> ([],   t,  [])

typeForm :: Type -> ([(Symb,Type)],Cat,[Term])
typeForm t =
 case t of
  Prod x a b  -> ((x,a):x', b', yy) where (x',b',yy) = typeForm b
  Ground c yy -> ([]      , c,  yy)
  _           -> error "no normal form of type"

valType :: Type -> Type
valType t = case t of 
              Prod x a b -> valType b 
              _          -> t

valCat :: Type -> Cat
valCat t = case typeForm t of (_,c,_) -> c

termForm2Term :: ([Symb],Term,[Term]) -> Term
termForm2Term (xx,a,bb) = abss xx (apps a bb)

typeForm2Type :: ([(Symb,Type)],Cat,[Term]) -> Type
typeForm2Type (xx,c,bb) = prods xx (Ground c bb)

apps :: Term -> [Term] -> Term
apps c yy = case yy of { y:ys -> apps (App c y) ys ; [] -> c }

abss :: [Symb] -> Term -> Term
abss xx b = case xx of { x:xs -> Abs x (abss xs b) ; [] -> b }

prods :: [(Symb,Type)] -> Type -> Type
prods xx b = case xx of { (x,a):xs -> Prod x a (prods xs b) ; [] -> b }

-- Grammar

type Grammar = (Lin,Theory)
type Theory  = ([(Ident,CatDef)],[VarSpec],[(Ident,Rule)],[Definition])
type Lin     = (Tokenizer,[(Ident,[(Tag,[TagType])])],[(Ident,OpDef)])
type CatDef  = (Context,[TagType],[TagType],Int)
type VarSpec = ([Ident],[Cat])

-- information for concrete syntax

data TagType   = TagType String deriving (Eq,Read,Show)
data Tag       = Tag String     deriving (Eq,Read,Show)

data TagExp    = TagCons Tag 
               | TagVar Ident 
               | NewTagExp Tag [TagExp] --- 28/2/2000
                deriving (Read,Show)

data Feature   = FeatAtom TagExp
               | FeatArg Int Int
               | FeatApp FeatOp [Feature] deriving (Read,Show)
data FeatOp    = FeatOpId Ident 
               | FeatOpCase [([TagExp],Feature)] deriving (Read,Show)

data Str       = StrCons [String] [([String],[String])]
               | StrComb Str Junct Str
               | StrArg Int Int [Feature]
               | StrBound Int Int
               | StrApp StrOp [Str] [Feature] Int --- 28/2/2000
               | StrLet Ident OpDef Str 
               | StrNewArg Ident [Feature] --- 28/2/2000
                deriving (Read,Show)
data StrOp     = StrOpId Ident
               | StrOpCase [([TagExp],[Str])] deriving (Read,Show)
type Junct     = Bool

type Entry     = (StrOp,[Feature])
type Rule      = (Type,Entry)

data OpDef     = DefStrOp  [TagType] StrOp
               | DefFeatOp [TagType] TagType FeatOp
               | DefNewOp  [(Ident,NewLinType)] [TagType] StrOp --- 28/2/2000
                deriving (Read,Show)

type NewLinType = ([TagType],Int) --- 28/2/2000

emptyGrammar = ((Tokenizer "mini" [],[],[]),([],[],[],[]))

numSymb :: String -> Int -> Symb
numSymb s n  = Symb (s ++ show n)

lookupCat :: Grammar -> Cat -> CatDef
lookupCat gr@(_,(cats,_,_,_)) (Cat c) = lookupDefault ([],[],[],1) c cats 

-- Tokenization

data Tokenizer = Tokenizer String [String] deriving (Read,Show) 

composeTokenizers :: Tokenizer -> Tokenizer -> Tokenizer
composeTokenizers (Tokenizer _ ss) (Tokenizer t ss') = Tokenizer t (ss ++ ss')


-- printing


