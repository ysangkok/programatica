module CustomCommands where
import Operations
import Grammar
import SymbolTable
import Macros
import TC () --- req. by hbc
import ComputeTerm (computeTerm)
import Paraphrases (mkParaphrases)
import TypeCheck
import Char (isDigit) ---

-- user-defined functions for command interpreter. AR 13/6/2000
type CommandId = String

-- term commands are used for computations, semantic actions, etc

termCommand :: AbstractST -> CommandId -> Trm -> [Trm]
termCommand abs comm trm = maybe [trm] ($ trm) (lookup comm (allTermCommands abs))

allTermCommands :: AbstractST -> [(CommandId, Trm -> [Trm])]
allTermCommands abs =
 [
  ("identity",    (:[]) . id),
  ("compute",     (:[]) . computeTerm abs),
  ("paraphrases", mkParaphrases abs),
  ("typecheck",   typeCheckTerm abs)
-- add your own term commands here
 ]


-- string commands are used for preprocessing before parsing and 
-- postprocessing after linearization
--- would be good to have a clear div of labour between these and (un)tokenization
--- perhaps just eliminate hard-wired tokenization ?!

stringCommand :: CommandId -> String -> String
stringCommand comm s = maybe s ($ s) (lookup comm allStringCommands)

allStringCommands :: [(CommandId, String -> String)]
allStringCommands =
 [
  ("identity",  id),
  ("erase",     const ""),
  ("take100",   take 100),
  ("format",    lineFormat),
  ("unformat",  lineUnFormat),
  ("latexfile", mkLatexFile)
-- add your own string commands here
 ]


txtCustomCommands = 
 "Available term commands (i.e. arguments of tc):\n  " ++ 
 unwords (map fst (allTermCommands emptyAbstractST)) +++++
 "Available string operations (i.e. arguments of so):\n  " ++ 
 unwords (map fst allStringCommands)

-- example, to be moved somewhere else:

-- string operations for formatting:
-- mark new lines by "##" and n-step indentation by "#n# (n = 0..9)
lineFormat s = case s of
  '#':'#':' ':cs -> '\n' : lineFormat cs
  '#':'#':cs -> '\n' : lineFormat cs
  '#':i:'#':cs | isDigit i -> replicate (read [i]) ' ' ++ lineFormat cs
  c:cs -> c  : lineFormat cs
  _ -> s

lineUnFormat s = case s of
  '\n':[] -> []
  '\n':cs -> '#':'#':' ':lineUnFormat cs
  c:cs -> c : lineUnFormat cs
  _ -> s

mkLatexFile s = begindocument +++++ s +++++ enddocument
