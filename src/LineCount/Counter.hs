{-# LANGUAGE UnicodeSyntax #-}
module LineCount.Counter
  ( countAll
  , countLine
  ) where


import           Control.Monad.State.Lazy
import           Control.Monad.Trans.Maybe
import           Data.Foldable
import           Data.Maybe
import           LineCount.Base
import           LineCount.Counter.Base
import           LineCount.Counter.Values
import           LineCount.Profile


countLine ∷ MainOptions → Profile → String → State CounterState CalcResult
countLine opts prof =
  fmap (fromMaybe mempty) . runMaybeT . msum . combinedFunction
  where
    combinedFunction = sequenceA (map ((($ prof) . ($ opts)) . unCounter) counterChain)


countAll ∷ MainOptions → Profile → [String] → CalcResult
countAll opts profs = fold . flip evalState emptyCS . traverse (countLine opts profs)
