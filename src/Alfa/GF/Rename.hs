module Rename where

import Operations
import Grammar
import Macros
import SymbolTable

-- renaming identifiers. AR 4/2/2000 -- 4/3

-- both dist. constants from variables and make identifier unique. 2/3/2000

--- the following functions look monadic enough to be made monadic...
--- ... and they are now very painful to work with...

renameInfo :: AbstractST -> (Ident,IdentInfo) -> (Ident,IdentInfo)
renameInfo bin entry@(const@(Ident (_,(def,_))),info) = case info of
  IdentCat cont -> (const,IdentCat (fst (renameContext def gst [] (cont,0))))
  IdentFun typ  -> (const,IdentFun (ren typ))
  IdentType typ -> (const,IdentType (ren typ))
  IdentDef pts  -> (const,IdentDef (map renp pts)) 
  IdentData cc  -> (const,IdentData (map renf cc))
 where
   gst    = onlyAbsST bin
   conss  = Just bin
   ren t  = fst (renameTerm def gst conss [] (t,0))
   renf c = case ren (Cons c) of --- this is awful. AR 1/11/2000
              Cons c' -> c'
              _ -> c
   renp (patt,trm) = case ren (Cases [([patt],trm)]) of Cases [([p],t)] -> (p,t)

renameLInfo :: GrammarST a -> (Ident,IdentLInfo a) -> (Ident,IdentLInfo a)
renameLInfo bin entry@(const@(Ident (_,(def,_))),info) = case info of
  IdentTagType params -> 
     (const, IdentTagType (fst (renameMany renameParam (params,0))))
  IdentTag typ -> (const,IdentTag (ren0 typ))
  IdentLintype typ -> (const,IdentLintype (ren0 typ))
  IdentLType typ -> (const,IdentLType (ren0 typ))
  IdentOper typ trm -> let (typ',mx')  = ren (typ,0)
                           (trm',mx'') = ren (trm,mx')
                       in (const,IdentOper typ' trm')
  IdentDefault vars trm -> mkLin vars trm
  IdentLin vars trm -> mkLin vars trm
  IdentVar cats -> let renCat cat = case ren0 (Var cat) of
                                      Cons cat' -> cat' --- 
                                      _ -> cat
                       cats' = map renCat cats
                   in  (const,IdentVar cats')
  _ -> entry
 where 
   rens = renameTerm def bin Nothing
   ren  = rens []
   ren0 t = fst (ren (t,0))

   renameParam ((tag,cont),mx) =
             let (Cons tag',mx') = ren (Var tag, mx) ---
                 (cont',mx'') = renameContext def bin [] (cont,mx')
             in  ((tag',cont'),mx'')
   mkLin vars trm =
     let vars'    = [Ident (x,(def,i)) | (Ident (x,_),i) <- zip vars [1..]]
         (trm',_) = rens vars' (trm, length vars)
     in (const,IdentLin vars' trm')


renameTermInContext :: AbstractST -> [Ident] -> Trm -> (Trm,Int)
renameTermInContext abs@(_,mx) vs trm = 
  renameTerm (mx+1) (onlyAbsST abs) (Just abs) vs (trm,0)

renameTermA :: AbstractST -> Trm -> (Trm,Int)
renameTermA abs = renameTermInContext abs []

renameTerm :: Int -> GrammarST a -> Maybe ((BinTree (Ident,i),k)) -> 
  [Ident] -> (Term a,Int) -> (Term a,Int)
renameTerm def gst@(abs,cnc) constructors bounds (trm,mx) = case trm of
  Var x -> (renameVar x, mx)
  Meta (MetaSymb (cat,s)) -> (Meta (MetaSymb (renameCons cat, s)), mx)
  App x y -> let ([x',y'],mx') = rens ([x,y],mx) 
             in (App x' y', mx')
  Abs (Ident (x,(_,i))) b -> let x' = Ident (x,(def,mx+1)) 
                                 (b',mx') = renameInScope [x'] (b,mx)
                             in (Abs x' b', mx')                               
  Prod x a b -> let ([(x',a')],mx') = renameContext def gst bounds ([(x,a)],mx)
                    (b',mx'') = renameInScope [x'] (b,mx')
                in (Prod x' a' b', mx'')
  Typed x y -> let ([x',y'],mx') = rens ([x,y],mx) 
               in (Typed x' y', mx')
  Cases cc -> (Cases [(pp',fst ( renameInScope xs (t,mx)) ) | 
                        (pp,t) <- cc, let (pp',xs) = renamePatts bounds pp],
               sum (map (length . fst) cc)) --- max counted separ. for each case
  Select f aa -> let (f',mx') = ren (f,mx)
                     (aa',mx'') = rens (aa,mx')
                 in (Select f' aa', mx'')
  Table aa v -> let (aa',mx') = rens (aa,mx)
                    (v',mx'') = ren (v,mx')
                in (Table aa' v', mx'')
  LiT t  -> let (t',mx') = ren (t,mx) in
                   (LiT t', mx')
  Project t i -> let (t',mx') = ren (t,mx) in
                   (Project t' i, mx')
  Record r -> let (ll,tt) = unzip r
                  (tt',mx') = rens (tt,mx)
              in (Record (zip ll tt'),mx')
  RecType r -> let (ll,tt) = unzip r
                   (tt',mx') = rens (tt,mx)
               in (RecType (zip ll tt'),mx')
  UpdRecord r (l,v) -> let (r',mx1) = ren (r,mx)
                           (v',mx2) = ren (v,mx1)
                       in (UpdRecord r' (l,v'),mx2)
  UpdRecType r (l,v) -> let (r',mx1) = ren (r,mx)
                            (v',mx2) = ren (v,mx1)
                        in (UpdRecType r' (l,v'),mx2)
  Let dd t -> let (dd',mx') = renameLocalDefs bounds (dd,mx)
                  (t',mx'') = renameInScope [x' | (x',_,_) <- dd'] (t,mx')
              in (Let dd' t', mx'')
  Concat x y -> let ([x',y'],mx') = rens ([x,y],mx) in 
                   (Concat x' y', mx')
  Glue x y -> let ([x',y'],mx') = rens ([x,y],mx) in 
                   (Glue x' y', mx')
  Alts d (t,tt) -> let (vv,cc) = unzip tt
                       (t',mx1)  = ren (t,mx)
                       (cc',mx2) = rens (cc,mx1)
                       (vv',mx') = rens (vv,mx2)
                   in (Alts d (t', zip vv' cc'), mx')
  Strs tt -> let (tt',mx') = rens (tt,mx) in 
                   (Strs tt', mx')
  _ -> (trm,mx)
             -- assuming incoming constants are all marked as Var
 where
   renb = renameTerm def gst constructors
   ren  = renb bounds
   rens = renameMany ren

   renameInScope xs (t,m) = renb (xs ++ bounds) (t,m + length xs)

   renamePatts xs [] = ([],xs)  -- different occ's of a var are renamed different
   renamePatts xs patts = case patts of
     PattVar x : pp -> case lookupConstr x of
       Ok c -> (PattCons c [] : pp', xs') where (pp',xs') = renamePatts xs pp
       _ -> (PattVar x' : pp', xs')
                 where
                   (pp',xs') = renamePatts (x':xs) pp
                   x' = Ident (symid x,(def,mx+length xs + 1)) ---awful use of Ident
     PattCons c aa : pp -> (PattCons c' aa' : pp', xs'') where
       (pp',xs'') = renamePatts (xs' ++ xs) pp
       (aa',xs') = renamePatts xs aa
       c' = case renameVar c of Cons i -> i ; Var i -> i --- Var i should not happen
     PattRec r : pp -> (PattRec (zip ls aa') : pp', xs'') where
       (ls,aa) = unzip r
       (pp',xs'') = renamePatts (xs' ++ xs) pp
       (aa',xs') = renamePatts xs aa

   renameVar var@(Ident (x,(_,i))) = 
     case lookup x [(v,k) | Ident (v,(_,k)) <- bounds] of 
       Just j -> Var (Ident (x,(def,j)))
       _ -> case lookupConsTree var of
         Ok (c',_) -> Cons c'
         _ -> Var var
	 
   renameCons c = 
     case lookupConsTree c of
       Ok (c',_) -> c'     
       _ -> strangeIdent (symid c) --- cannot happen (but does)

   lookupConstr c = case constructors of
     Just cc -> case look c cc of
                  Ok ci -> return (fst ci)
                  _ -> Bad "look constr" 
     _ -> case lookupConsTree c of
            Ok (c', Right (IdentTag _)) -> Ok c'
            _ -> Bad "lookupConstr"

   lookupConsTree c = 
     case look c abs of
        Ok (c',i) -> Ok (c', Left i)
        _ -> case look c cnc of
               Ok (c',IdentVar _) -> Bad ("Not a constant" +++ show c)
               Ok (c',i) -> Ok (c', Right i)
               Bad s -> Bad s

   look f st = lookupTreeEq show eqStrIdent f 
                           (mapTree (\ (x,y) -> (x,(x,y))) (fst st))

   renameLocalDefs bs (dd,mx) = case dd of
     [] -> ([],mx)
     (x,a,t):ds -> let x' = Ident (symbolOfIdent x,(def,mx+1))
                       (a',mx')    = renb bs (a, mx + 1)
                       (t',mx'')   = renb bs (t, mx')
                       (ds',mx''') = renameLocalDefs (x':bs) (ds, mx'')
                   in ((x',a',t'):ds', mx''')

renameContext :: Int -> GrammarST a -> [Ident] -> (Context a,Int) -> (Context a,Int)
renameContext def bin bounds (cont,mx) = case cont of
  [] -> ([],mx)
  (x,a):hs -> let x' = Ident (symbolOfIdent x,(def,mx+1))
                  (a',mx') = renameTerm def bin Nothing bounds (a, mx + 1)
                  (hs',mx'') = renameContext def bin (x':bounds) (hs, mx')
              in ((x',a'):hs', mx'')

renameMany :: ((a,Int) -> (a,Int)) -> ([a],Int) -> ([a],Int)
renameMany rename (trms,mx) = case trms of
  [] -> ([],mx)
  t:ts -> let (t',mx') = rename (t,mx) 
              (tt',mx'') = renameMany rename (ts,mx')
          in (t':tt', mx'')

