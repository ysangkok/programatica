module EmbedDrawing where
import Fud
import AllFudgets(mapLabelDrawing,mapEither)
import Embed
import Annot
import EditFunc

embedDrawing f emb = mapLabelDrawing embAnnot
  where
    e = embed emb
    e1 = embed1 emb
    --ed = embed $ Emb (embedDrawing (revEmb emb)) (embedDrawing emb) --horror!
    embAnnot (Annot u b) = Annot (f u) (e b) --(map embMenu m) 


embedMenu emb (EditMenu m) = EditMenu (map embMenu m)
  where
    embMenu (ef,s) = (mapEither id embEditFunc ef,s)

    embEditFunc (EditLocal mod2) = EditLocal (embMod2 mod2)
    embEditFunc (EditParent pmod2) = EditParent (embMod2.pmod2)
    -- ...

    embMod2 (Left m1) = error "x"--Left (ed m1)
    embMod2 (Right m2) = Right (embedMod emb m2)
