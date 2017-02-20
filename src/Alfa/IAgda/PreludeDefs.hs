-- | Contains built in prelude definitions

module PreludeDefs(preludeDefs) where
import CSyntax
import MiscId
import Position 
import Id 
import MiscId


set :: CExpr
set = cSet noPosition

ctype :: CExpr
ctype = cType noPosition


boolDef :: CLetDef
boolDef = 
  CSimple (CDef [] (Cdata boolId [] Nothing [(trueId,[]),(falseId,[])]))


{-
trueDef :: CLetDef
trueDef = CSimple (CDef [] (CValueT trueId [] (CVar boolId) 
          (CCon trueId (CVar boolId))))
 
falseDef :: CLetDef
falseDef = CSimple (CDef [] (CValueT falseId [] (CVar boolId) 
          (CCon falseId (CVar boolId))))
-}


boolDefs :: [CLetDef]
boolDefs = [boolDef]


integerDef :: CLetDef
integerDef = CSimple (CDef [] (CNative integerId (cSet noPosition)))


rationalDef :: CLetDef
rationalDef = CSimple (CDef [] (CNative rationalId (cSet noPosition)))



listDef :: CLetDef
listDef = CSimple (CDef [] d)
   where d :: CDefn 
         d = Cdata listId [CArg [(False,a)] set] Nothing [nilc,consc]
         nilc :: CSummand 
         nilc = (nilId,[])
         consc :: CSummand
         consc = (consId,[CArg [(False,x)] aExp,CArg [(False,xs)] listExp ])
         listExp :: CExpr
         listExp = cApply (CVar listId) [(False,aExp)]
         aExp = CVar a
         set = cSet noPosition
         a = hypTypeId noPosition 
         x = listVarHeadId 
         xs = listVarTailId
         list = CVar listId




charDef :: CLetDef
charDef = CSimple (CDef [] (CNative charId  (cSet noPosition)))

stringDef :: CLetDef
stringDef = CSimple (CDef [] (CValueT stringId [] (cSet noPosition) (cApply 
    (CVar listId) [(False,CVar charId)])))


{-


setoidDef =  CSimple (CDef [] d)
    where d = CValueT setoidId [] ctype e
          e = CProduct noPosition (map cSign [(elemId,set),(equalId,equalT),(refId,refT),(symId,symT),(tranId,tranT)])
          cSign (i,e) = CSign [i] e
          mkElemAbs b xs e = cUniv [CArg (map ((,) b) xs) elemVar] e
          mkEqual x1 x2 = CBinOp (CVar x1) equalId (CVar x2)
          elemVar = CVar elemId
          equalT = CArrow False elemVar (CArrow False elemVar set)
          refT = mkElemAbs False [varId] (mkEqual varId varId)
          symT = mkElemAbs True [varId1,varId2] ( CArrow False (mkEqual varId1 varId2) (mkEqual varId2 varId1))
          tranT = mkElemAbs True [varId1,varId2,varId3] ( CArrow False (mkEqual varId1 varId2) 
                     (CArrow False (mkEqual varId2 varId3) (mkEqual varId1 varId3)))

elDef =  CSimple (CDef [] d)
     where d = CValueT elId args t e 
           args = [CArg [(False,typeVarId)] (CVar setoidId)] 
           t = set 
           e = CSelect (CVar typeVarId) elemId

eqDef =  CSimple (CDef [] d)
     where d= CValueT eqId args t e 
           args = [CArg [(True,typeVarId)] (CVar setoidId)] 
           t = CArrow False elX (CArrow False elX set)
           e = Clam args' body
           args' = CArg [(False,varId1),(False,varId2)] elX
           body = CApply (CSelect (CVar typeVarId) equalId) 
                         [(False,CVar x)| x <- [varId1,varId2]]
           elX = CApply (CVar elId) [(False,CVar typeVarId)]


-}

preludeDefs :: [CLetDef]
preludeDefs = boolDefs ++ [integerDef,charDef,listDef,stringDef] 

-- ++ [integerDef,rationalDef,listDef,charDef,stringDef]

