module LexSymbol where

class Show s => Symbol s where
  lexToInt :: s->Int
