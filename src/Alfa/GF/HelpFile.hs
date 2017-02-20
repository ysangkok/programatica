module HelpFile where

txtHelpFile =
  "\nAvailable GF commands and their options:" ++
  "\n" ++
  "\n(last updated 26/4/2001)" ++
  "\n" ++
  "\ni, import: i opt* File " ++
  "\n      -gfc   already optimized - skip compilation and type checking" ++
  "\n      -ebnf  EBNF format" ++
  "\n      -cf    Context-free format" ++
  "\n      -old   Old GF format" ++
  "\n      -s     silent: don't give error messages" ++
  "\n      -latex LaTeX: create a LaTeX file from the grammar" ++
  "\n      -w     whole grammar: don't minimize by removing oper and lintype definitions" ++
  "\n" ++
  "\nrf, read file: rf File" ++
  "\n      -lines read each line separately" ++
  "\n      -trm   parse as term input" ++
  "\n" ++
  "\nwf, write file: wf File" ++
  "\n" ++
  "\naf, append file: af File" ++
  "\n" ++
  "\nsa, speak aloud: sa Voice? String" ++
  "\n  uses the festival speech generator." ++
  "\n  Voice is passed to festival as the value of its --language flag." ++
  "\n  Omitting it gives the default voice (often British English)." ++
  "\n" ++
  "\nws, write string: ws String" ++
  "\n" ++
  "\nl, linearize: l opt* (-- Language)? Term" ++
  "\n      -table  show full inflection table as list" ++
  "\n      -latex  show full inflection table in LaTeX form" ++
  "\n" ++
  "\np, parse: p opt* (-- Language)? Cat String" ++
  "\n      -n     non-strict: tolerates morphological errors" ++
  "\n      -1     show just the first parse" ++
  "\n      -lit   treat unknown words as literal strings" ++
  "\n      -ign   ignore unknown words when parsing" ++
  "\n      -ear   Earley algorithm" ++
  "\n      -td    Combinator parsing" ++
  "\n      -td2   Combinator parsing tolerant for implicit left recursion" ++
  "\n      -pp    Parsek combinator parsing" ++
  "\n      -xml   show result in XML" ++
  "\n      -old   show result in old GF notation" ++
  "\n      -hs    show result by Haskell's show" ++
  "\n      -raw   return context-free terms in raw form" ++
  "\n      (default is Chart parsing)" ++
  "\n" ++
  "\nt, translate: t opt* Language Language Cat String" ++
  "\n      options: same as parse" ++
  "\n" ++
  "\nma, morphological analysis: ma opt* (-- Language)? String" ++
  "\n      -morpho  analyser optimized for big input (i.e. >100 words)" ++
  "\n" ++
  "\nrq, random questions: rq opt* Language Language Cat" ++
  "\n" ++
  "\nmq, morphological questions: mq opt* (Language) Cat" ++
  "\n" ++
  "\nte, translation exercises: te opt* Language Language Cat Int" ++
  "\n" ++
  "\nme, morphology exercises: me opt* (Language) Cat Int" ++
  "\n" ++
  "\ngr, generate random: gr opt* Int Cat" ++
  "\n" ++
  "\npg, print grammar: pg opt* (-- Language)?" ++
  "\n      -cf    Context-free" ++
  "\n      -opt   Optimized format" ++
  "\n      -abs   Abstract grammar" ++
  "\n      -hs    Haskell-readable object (uses show)" ++
  "\n      -hsa   Haskell module with the abstract syntax (using show)" ++
  "\n      -hsf   Haskell module with the abstract syntax as data type definitions" ++
  "\n      -ws    words of the grammar" ++
  "\n      -xml   XML DTD" ++
  "\n      -latex LaTeX file" ++
  "\n" ++
  "\ncp, cf properties: cp opt* Int (-- Language)?" ++
  "\n      Inspects left recursion and empty productions." ++
  "\n      Int gives the depth to which left recursion is searched." ++
  "\n" ++
  "\nrp, reduce parser: rp (-- Language)? fun+" ++
  "\n      Remove context-free rules for named functions from parser." ++
  "\n" ++
  "\nph, print history: ph opt*" ++
  "\n" ++
  "\neh, execute history: opt* File" ++
  "\n" ++
  "\nps, parse session: ps opt* (--Language)? Cat" ++
  "\n      options: same as parse" ++
  "\n" ++
  "\nts, translate session: ts opt* Language ( Language+ ) Cat" ++
  "\n      options: same as parse, plus" ++
  "\n      -f Fudget (necessary for Unicode)" ++
  "\n" ++
  "\nes, edit session: es opt*" ++
  "\n      -f Fudget (necessary for Unicode)" ++
  "\n" ++
  "\nsc, system call: sc String" ++
  "\n" ++
  "\ntc, term command: tc Ident Term" ++
  "\n" ++
  "\nso, string operation: so Ident String" ++
  "\n" ++
  "\ne, empty: e" ++
  "\n" ++
  "\nh, help: h" ++
  "\n" ++
  "\nq, quit: q" ++
  "\n" ++
  "\nString arguments are given in quotes, e.g. p Phrase \"hello\"" ++
  "\n" ++
  "\nCommands can be piped in the Unix style: for instance," ++
  "\n  p --Eng Phrase \"good morning\" | l --Fra" ++
  "\n  -- parses the string \"good morning\" as a Phrase in Eng, and linearizes" ++
  "\n     the result in Fra" ++
  "\n  gr 2 Phrase | l | sa " ++
  "\n  -- generates two random Phrases, linearizes them, and speaks aloud" ++
  "\n  h | so take100 | sa " ++
  "\n  -- speaks aloud the first 100 characters of this help text" ++
  "\n" ++
  "\nThe word trace prefixed to a command line causes GF to print the value" ++
  "\nof each step in a pipe. For instance, \"trace gr 1 S | l | p S | l\"" ++
  "\ncan be used for testing what expressions of category S are ambiguous." ++
  "\n" ++
  "\nA command line consisting of an integer repeats a command back in the history." ++
  "\nFor instance, 0 repeats the last command, 1 the second-last, etc." ++
  "\n" ++
  []