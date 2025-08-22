module Modulus.FE.CSS
  ( mainBackgroundCSS
  ) where

import Web.Atomic (ClassName)

mainBackgroundCSS :: ClassName
mainBackgroundCSS =
  "min-h-screen bg-gradient-to-br \
  \from-slate-900 via-purple-900/20 \
  \to-slate-900 flex items-center justify-center p-4"
