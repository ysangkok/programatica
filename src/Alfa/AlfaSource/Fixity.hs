module Fixity where

data SyntacticContext var
  = TopLevel -- at the top level or anywhere where brackets are never needed
  | Atomic   -- arguments in applications and other places where all compound
             -- expressions always need brackets
  | InfixArg Assoc Precedence OpSide
  | NDGoal var -- goal in proof style notation.
  | Subgoal    -- subgoal in proof style, special printing of abstractions.
--  | LambdaBody -- body of a lambda abstraction. Special printing.
--  | ArrowRhs -- rhs of an function type arrow. Special printing.
  deriving (Eq,Show)

data OpSide = LHS | RHS deriving (Eq,Show)

data Fixity
  = Nonfix
  | Infix Assoc Precedence
  | Distfix3 Assoc Precedence -- small arg above
  | Distfix3b Assoc Precedence -- small arg below
  | Distfix4 Assoc Precedence -- small args above and below
  | Distfix Assoc Precedence -- distribution specified by "_" in the name
  | Postfix Precedence
-- | Prefix  -- Precedence ?
  | Quantifier Bool -- show domain?
  | Big
--  | BigQuantifier
  | Tuple
  | Fraction
  | ProofGoal
  -- more...
  deriving (Eq,Show,Read)

-- isDistfix determines if an identifer needs brackets in an Atomic context
isDistfix (Infix _ _) = True
isDistfix (Distfix3 _ _) = True
isDistfix (Distfix3b _ _) = True
isDistfix (Distfix4 _ _) = True
isDistfix (Distfix _ _) = True
isDistfix (Postfix _) = True
isDistfix _ = False

data Assoc
  = LeftAssoc | RightAssoc | NonAssoc | Assoc
  deriving (Eq,Show,Read)

type Precedence = Int   -- Allow 0..9 only?

needParen :: SyntacticContext var -> Assoc -> Precedence -> Bool
needParen context assoc prec =
  case context of
    InfixArg assoc' prec' side ->
      case prec `compare`  prec' of
	LT -> True
	EQ -> not (assoc==assoc' && side `onAssocSide` assoc)
	GT -> False
    Atomic -> True
--    ArrowRhs -> False
--    LambdaBody -> False
    TopLevel -> False
    NDGoal _ -> False
    Subgoal  -> False

side `onAssocSide` assoc =
  case (side,assoc) of
    (_,Assoc) -> True
    (LHS,LeftAssoc) -> True
    (RHS,RightAssoc) -> True
    _ -> False
