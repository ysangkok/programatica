# programatica
Thomas Hallgren's Haskell source browser, modernized for GHC 8

To use

* stack build, it will fail, but build hsLexer
* run hsLexer use output redirection to generate Lexer/HsLex.hs
* stack build again
* run `cpphs Paths_programatica.hs |  tail -n +5` and save it to Paths_programatica.hs to remove `#` and `{-#` lines 
* make new directory, run `main new <list of files>` and then `main webpages`

You now have something similar to hs2html.

See also https://github.com/RefactoringTools/HaRe/tree/master/old/tools/hs2html or http://code.haskell.org/~malcolm/hscolour/ 
