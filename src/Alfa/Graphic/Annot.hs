module Annot where
import Fud(Drawing,Gfx,labelD)

data Annot u a = Annot u ([a]->a)

type ADrawing u a = Drawing (Annot u a) Gfx

annotD :: u -> ([a]->a) -> ADrawing u a -> ADrawing u a
annotD descr build = labelD (Annot descr build)

--annot  (descr,build,menu) = labelD (Annot descr build (mapEdFunc editLocalDrawing menu))
--annota (de,b,m) = labelD (Annot de b (mapEdFunc editLocalAbs m))
