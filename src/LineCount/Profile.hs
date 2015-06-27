module LineCount.Profile
  ( Profile(..)
  , profiles
  , providedProfiles
  ) where


import           Control.Arrow
import           LineCount.Profile.Base
import           LineCount.Profile.Prfs
import qualified Data.Map.Lazy as Map
import           Control.Monad


providedProfiles :: [String]
providedProfiles = map canonicalName __all


profiles :: Map.Map String Profile
profiles = Map.fromList $ join $ map (\p -> map (id &&& const p) $ acceptedNames p) __all
