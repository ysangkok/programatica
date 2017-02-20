module AlfaPluginKit(
  module AlfaPluginKit,
  module AlfaPlugin,
  module AlfSyntax,
  module AlfaText,
  module UAbstract,
  module UAnnots,
  module AbstractOps,
  module ProofEngine,
  --State,
  module ProofmonadOps,
  module DrawAlf)
 where

import AlfaPlugin
import AlfSyntax
import AlfaText
import UAbstract
import UAnnots
import AbstractOps
import ProofEngine hiding (Env) -- the same type synonym as in AlfaPlugin
import ProofmonadOps
import DrawAlf(drawTopSyntax)
import Fud(Gfx(G), vboxlD, hboxcD)

displaySyntax s =
  GetDrawOptions $ \ altdispfs drOpts ->
  DisplayGfx (G (drawTopSyntax altdispfs drOpts s))
displaySyntax2 s =
  GetDrawOptions $ \ altdispfs drOpts ->
  DisplayGfx (G$ vboxlD (map hboxcD (map (map (drawTopSyntax altdispfs drOpts)) hs )))
     where hs = [[EVar (Var a),  b] | (a, b) <- s]



          
