module LineCount.Counter.Base where


import Control.Monad.State.Lazy
import Control.Monad.Trans.Maybe
import LineCount.Base
import LineCount.Profile


data CounterState = CounterState { currentDelimiter :: Maybe (String, String) } deriving (Eq, Show)


emptyCS :: CounterState
emptyCS = CounterState { currentDelimiter = Nothing }


newtype Counter = Counter { unCounter :: MainOptions -> Profile -> String -> MaybeT (State CounterState) CalcResult }


instance Monoid Counter where
  mempty = Counter (\_ _ _ -> mzero)
  mappend (Counter func1) (Counter func2) = Counter newfunc
    where
      newfunc opts profile input = func1 opts profile input `mplus` func2 opts profile input
