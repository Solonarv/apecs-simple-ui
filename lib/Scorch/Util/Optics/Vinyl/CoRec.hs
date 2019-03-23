module Scorch.Util.Optics.Vinyl.CoRec where

import Control.Lens.Prism
import Data.Vinyl.CoRec

rprism :: (a ∈ ss, b ∈ ts, RDelete a ss ⊆ ts)
       => Prism (CoRec f ss) (CoRec g ts) (f a) (f b)
rprism = prism (\cr@(CoRec fa) -> case asA' @a cr of Nothing -> CoRec fa; Just fa -> fa)
               (\fb -> CoRec fb)