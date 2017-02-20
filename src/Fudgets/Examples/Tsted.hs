-- Tsted - A simple text editor based on editorF, the text editor fudget.

import AllFudgets
import DialogueIO hiding (IOError) -- Exit

main = fudlogue $ shell "Emagn" emagnF
shell s = shellF s . spacer1F (sizeS (Point 500 250))

emagnF = startupF (take 1 args) $
         loopThroughRightF
             ctrlF
            (vBoxF (menuBarF >+< scrollF editorF))

menuBarF = spacer1F (hAlignS aLeft) fileMenuF

data FileCmd = FileNew | FileOpen | FileSaveAs | FileQuit deriving (Eq,Ord,Enum)

fileMenuF =
    stripEither>^=<
    idRightF (inputPopupF "File Selection" oldFilePickF Nothing >=^< pshowcmd)>=^<
    route >==<
    simpleMenuF menuFont " File " alts showcmd
  where alts = [FileNew ..]
        showcmd FileNew = "New"
        showcmd FileOpen = "Open..."
        showcmd FileSaveAs = "Save As..."
        showcmd FileQuit = "Quit"
        pshowcmd cmd = (Just (showcmd cmd),Nothing)
        route f | f==FileOpen || f==FileSaveAs = Left f
                | otherwise                    = Right (pshowcmd f,"") -- grr


ctrlF =
  getF $ \ msg ->
  case msg of
    Right name -> open name
    Left (Left ((Just cmd,_),name)) ->
      case cmd of
        "New"        -> putEd new ctrlF
        "Open..."    -> open name
        "Save As..." -> getText $ \ s -> writefile name s $ ctrlF
        "Quit"       -> hIOSuccF (Exit 0) nullF
    _ -> ctrlF
  where
    open name = readfile name $ \ s -> putEd (load s) $ ctrlF
    putEd = putsF . map (Left. Right)
    new = load ""
    load s = selectall++[EditReplace s]
    getText = putEd [EditGetText] . waitForF ans
       where ans (Left (Right (EditText s))) = Just s
             ans _ = Nothing

readfile  name k = hIOF (ReadFile name) $ \(Str s) -> k s
writefile name s = hIOSuccF (WriteFile name s)
