-- $Id: HsPatStruct.hs,v 1.29 2005/05/31 02:25:25 hallgren Exp $

module HsPatStruct where

import SrcLoc
import HsIdent
import HsLiteral
import HsFieldsStruct

data PI i p
    = HsPId (HsIdentI i)         -- Variables and nullary constructors
    | HsPLit SrcLoc HsLiteral    -- Literal
    | HsPNeg SrcLoc HsLiteral    -- only numeric literals can be negated
    | HsPSucc SrcLoc i HsLiteral -- the horrible n+k pattern -- integer literal
    | HsPInfixApp p i p          -- For example fx:xs
    | HsPApp i [p]               -- Constructor applications
    | HsPTuple SrcLoc [p]        -- Tuple pattern, (p_1,...,p_n)
    | HsPList  SrcLoc [p]        -- List pattern, [p_1,...,p_n]
    | HsPParen p                 -- (p)
    | HsPRec i (HsFieldsI i p)   -- C{f_1=p_1,...,f_n=p_n}
    | HsPAsPat i p               -- x@p
    | HsPWildCard                -- _
    | HsPIrrPat p                -- ~p
      deriving (Eq, Show)
