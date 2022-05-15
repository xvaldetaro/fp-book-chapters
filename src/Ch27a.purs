module Ch27a where

import ChalkStyles
import Effect.Class.Console
import Prelude

import ChalkStyles as ChalkStyles
import Data.Function.Uncurried (Fn2, runFn2)
import Effect (Effect)

foreign import _chalk :: Fn2 (Array Style) String String

chalk :: Array Style -> String -> String
chalk x y = runFn2 _chalk x y

test :: Effect Unit
test = do
  log $ chalk [ChalkStyles.red, ChalkStyles.bold] "placeholder"
