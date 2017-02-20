--- The 15-puzzle ---

import Fudgets
import List(sort)

main = fudlogue $ shellF (show n++" Puzzle") mainF

mainF = loopLeftF (ctrlSP0 >^^=< dynPlacerF puzzleF)

puzzleF = fst >^=< listF [(i,sqF i) | i<-[0..n]]
  where sqF 0 = holeF
        sqF i = buttonF' (setMargin 7) i

ctrlSP0 = ctrlSP [(x,y)|y<-[1..height],x<-[1..width]]

ctrlSP btns = putSP (Left $ puzzleP btns) $
              getSP $ ctrlSP . move btns

move btns i = if bx == hx || by == hy 
                 then (bx,by):[(shift bx hx x by hy y,
                                shift by hy y bx hx x)
                            | (x,y) <- tail btns]
                 else btns
  where (bx,by) = btns !! i
        (hx,hy) = btns !! 0
        shift b h c bo ho o = if bo == ho && ho == o
                              && c >= min b h && c <= max b h
                                then c + signum (h - b)
                                else c

puzzleP ps = permuteP (permutation ps) (matrixP' height Vertical 0)

permutation = map snd . sort . map swap . number 0

---

n = width*height-1
width = argReadKey "width" 4
height = argReadKey "height" 4
