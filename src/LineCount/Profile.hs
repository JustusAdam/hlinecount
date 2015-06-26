module LineCount.Profile
  ( Profile(..)
  , profiles
  ) where


import           Control.Arrow
import           LineCount.Profile.Base
import           LineCount.Profile.Prfs
import qualified Data.Map.Lazy as Map
import           Control.Monad


profiles = Map.fromList $ join $ map (\p -> map (id &&& const p) $ acceptedNames p) __all
