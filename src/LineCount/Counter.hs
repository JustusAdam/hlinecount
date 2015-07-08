module LineCount.Counter
  ( countAll
  , countLine
  ) where


import Control.Monad.State.Lazy
import Control.Monad.Trans.Maybe
import LineCount.Base
import LineCount.Profile
import Data.Maybe
import Data.Foldable
import LineCount.Counter.Values
import LineCount.Counter.Base


countLine :: MainOptions -> Profile -> String -> State CounterState CalcResult
countLine opts prof =
  fmap (fromMaybe mempty) . runMaybeT . msum . combinedFunction
  where
    combinedFunction = sequenceA (map ((\f -> f opts prof) . unCounter) counterChain)


countAll :: MainOptions -> Profile -> [String] -> CalcResult
countAll opts profs = fold . flip evalState emptyCS . mapM (countLine opts profs)
