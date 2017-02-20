import Fudgets
import SmileyF2

main = fudlogue $ shellF "tstSmiley" tstF


tstF = spacer1F centerS smileyF >==<
	radioGroupF [(mode,show mode) | mode<-[Sad ..Happy]] Happy
