module AlfaText where

-- Text with embedded syntactic objects:

data TextItem syntax
  = TSyntax syntax -- e.g., meta variables, or other embedded expressions
  | TPlain String
  | TSymbol (Maybe String) String -- first argument is an optional font name
  | TVar String -- variable name
  deriving (Eq,Show)

data Para syntax
   = PlainPara [TextItem syntax]
   | NestedPara [Para syntax]
   deriving (Show)

type Text syntax = [Para syntax] -- a text is a sequence of paragraphs

plaintext s = [PlainPara [TPlain w|w<-words l]|l<-lines s] :: (Text a)

syntaxtext :: a -> Text a
syntaxtext stx = [PlainPara [TSyntax stx]]
