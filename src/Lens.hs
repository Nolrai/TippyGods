module Lens (module Control.Lens, makeLenses) where

import Control.Lens hiding (makeLenses)
import Language.Haskell.TH

makeLenses :: Name -> DecsQ
makeLenses = makeLensesWith (set lensField (mappingNamer (pure . (<> "L"))) lensRules)
