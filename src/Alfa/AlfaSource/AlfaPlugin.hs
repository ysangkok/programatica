module AlfaPlugin where
import AlfSyntax
import AlfaText
import UAbstract(Module(..),Decls,Var,Exp,MetaVar)
import AbstractOps(isImportDecl)
import UAnnots(Position(..))
import DrawOptions(DrawOptions)
import qualified ProofEngine as PE
import Fud(Gfx)

#include "exists.h"

type PluginName = String -- assumed to be String in UAnnots

type Annotation = String
type Annotations = [Annotation]

type CompleteFunc = String -> [(String,String)]

data DeclsSource = Opened FilePath | Imported FilePath | Appended
isImported (Imported _) = True
isImported _ = False

type SyntaxText = Text Syntax
--type ExpText = Text Exp
syntext x = TSyntax . syn $ x

type MetaEnv = MetaVar->(Env,Exp) -- ctx & type of meta variables
type Env = [(Var,Exp)] -- type of variables that are in scope
type TextInputParser a = String -> Parsed a
type Parsed a =  Either ParseError a
type ParseError = (SourcePos,String)
type SourcePos = (Int,Int)
-- Error message is ((row,column),explanation), where row & column start at 0

unknownSourcePos = (0,0)::SourcePos -- hmm
sourcePos (Position _ r c) = (r,c)

-- An Alfa Plugin is a value of the following type:

data AlfaPlugin state =
  Methods {
    -- The name of the plugin, e.g., Dummy or GF:
    name :: PluginName,
    -- The initial state:
    state0 :: [FilePath] -> IO state,
    -- Functions to adjust the state when a new document is imported or opened.
    -- The string argument is the annotations produced by the save function:
    loadDecls :: DeclsSource->
                 state->Decls-> [(SourcePos,Annotation)] -> Parsed state,
    -- A function to save information along with a saved document
    save :: state -> Annotations,
    -- A function to add text to the info display and commands to the menu:
    describe :: Describe state,
    -- A list of named alternative display functions:
    altdisp :: state -> [(String,MetaEnv->Syntax->SyntaxText)],
    -- A list of named alternative text input parsers:
    altparse :: state -> [(String,TextInputParser Exp)], -- ??
    -- Pairs of file name extensions and functions for loading files
    -- in plugin defined formats:
    fileparsers::[(String,
		   state->Bool->FilePath->String -> Parsed (Module,state))]
  }

loadModule plug dsrc state = loadDecls plug dsrc state . rmImports
  where rmImports (Module ds) = filter (not.isImportDecl) ds

ifImported imp load dsrc = if isImported dsrc then imp else load

-- The function that describes the current selection.
type Describe state
  = state -> MetaEnv -> [Syntax] -> (Maybe String,Menu Syntax state)

-- Commands to add to the menu window:
type Menu syntax state = [(String,PluginCommand syntax state)]

-- Some commands the plugin can ask Alfa to execute:
data PluginCommand syntax state
  -- Manipulating the document:
  = Give Exp
  | Replace syntax
  | ReplaceRecheck syntax
  | ReplaceState state
  | TryGive Exp (Maybe String->PluginCommand syntax state)
  -- Input/Output
-- | ReadFile FilePath (String->...)
  | SaveFileAs (Maybe FilePath) String -- suggested file name, contents
  -- User interaction:
  | EXISTS(a) EditWith (EArgs syntax state EQV(a))
                       (EQV(a)->PluginCommand syntax state)
  | Menu (PluginMenu syntax state)
  | Message (Either String String) -- error or success
  | DisplayGfx Gfx -- display graphics in a separate window
  -- Hook for proof engine interaction
  | GetPEState (PE.State->PluginCommand syntax state)
  | GetDrawOptions (AltDispFuns->DrawOptions->PluginCommand syntax state)
  -- ... more

type AltDispFuns = [((PluginName,String),Syntax->SyntaxText)]

type PluginMenu syntax state =
  [(PluginCommand syntax state,(String,syntax))]

data EArgs syntax state a =
  EArgs { multiline::Bool,
	  prompt::String,
	  def::Maybe String, -- default
	  parser::TextInputParser a,
	  complete::CompleteFunc
	}


stringEdit prompt parser = EArgs False prompt Nothing parser (const [])
textEdit prompt parser = (stringEdit prompt parser){multiline=True}

editWith e prompt parser = EditWith $ e prompt parser

editWithText  = editWith textEdit
editWithString = editWith stringEdit
