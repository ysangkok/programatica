-- A very simple calculator
-- Compile with hbcxmake -contrib
import Fudgets
import ContribFudgets(helpBubbleF)

main :: IO ()
main = fudlogue (shellF "Counter" counterF)

counterF = intDispHelpF intDispF >==< absF (stateSP 0) >==< buttonsF
  where
    intDispHelpF =
      helpBubbleF "This display shows the current value of the counter."

buttonsF = hBoxF $ untaggedListF buttonlist
  where
    buttonlist = [bf "Add 1" (+1) "This button increments the counter.",
                  bf "Sub 1" (+(-1)) "This button decrements the counter.",
                  bf "Clear" (const 0) "This button resets the counter.",
                  bq "Quit" "This button terminates the program."]

    bf s f h = const f >^=< helpBubbleF h (buttonF s)
    bq s   h = quitF   >==< helpBubbleF h (buttonF' (setFont "-*-times-*-*-*-*-24-*-*-*-*-*-*-*") s)

type State = Int
type StateModifier = State -> State

stateSP :: State -> SP StateModifier State
stateSP startstate =
  let transformSP state =
         putSP state $
	 getSP $ \op ->
	 transformSP (op state)
  in transformSP startstate
