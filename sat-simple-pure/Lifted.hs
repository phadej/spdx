module Lifted where

import Data.Kind (Type)
import GHC.Exts  (UnliftedType)

type Lifted :: UnliftedType -> Type
data Lifted a = Lift a
