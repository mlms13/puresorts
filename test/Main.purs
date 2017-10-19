module Test.Main where

import Prelude
import Test.QuickCheck

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)

-- main :: forall e. Eff (console :: CONSOLE, random :: RANDOM, exception :: EXCEPTION | e) Unit
main :: ∀ e. Eff (console :: CONSOLE, random :: RANDOM, exception :: EXCEPTION | e) Unit
main = do
  quickCheck (\n -> n + 1 === 1 + n)
