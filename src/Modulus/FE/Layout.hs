{-
This module shall contain layouts for all pages
-}
module Modulus.FE.Layout (
    mainBackground
) where 

import Modulus.FE.CSS (mainBackgroundCSS)
import Web.Hyperbole
import Web.Atomic.CSS

mainBackground :: View c () -> View c ()
mainBackground = el ~ cls mainBackgroundCSS 
