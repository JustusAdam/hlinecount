{-# LANGUAGE UnicodeSyntax #-}
module LineCount.Counter.Base where


import           Control.Arrow
import           Control.Monad.State.Lazy
import           Control.Monad.Trans.Maybe
import           Data.Function
import           Data.Function.JAExtra
import           Data.Tuple.JAExtra
import           LineCount.Base
import           LineCount.Profile
import           Prelude.Unicode


data CounterState = CounterState
  { currentDelimiter ∷ Maybe (String, String)
  } deriving (Eq, Show)


emptyCS ∷ CounterState
emptyCS = CounterState { currentDelimiter = Nothing }


newtype Counter = Counter
  { unCounter ∷ MainOptions → Profile → String → MaybeT (State CounterState) CalcResult
  }


instance Monoid Counter where
  mempty = Counter (const3 mzero)
  mappend (Counter func1) (Counter func2) = Counter newfunc
    where
      newfunc = curry3 (uncurry mplus ∘ ((&&&) `on` uncurry3) func1 func2)
