module LineCount.Select
  ( selectProfile
  ) where

import           Control.Monad
import           Data.Foldable
import           Data.Function.JAExtra
import           LineCount.Base
import           LineCount.Profile.Base
import           System.FilePath


{-|
  Represents a step int the 'Profile' selection process. Results are 'mplus' chained,
  ergo from left to right the function to first return a 'Just' value selects the profile.
-}
newtype Selector = Selector { unSelector :: MainOptions -> [Profile] -> FilePath -> Maybe Profile }


instance Monoid Selector where
  mempty = Selector (const3 Nothing)
  mappend (Selector s1) (Selector s2) = Selector newfunc
    where
      newfunc opts profs path = s1 opts profs path `mplus` s2 opts profs path


defaultSelector :: Selector
defaultSelector = Selector (const func)
  where
    func profiles path = lookup (takeExtension path) $ prfsToAssocList profiles


selectorChain :: [Selector]
selectorChain =
  [ defaultSelector
  ]


selector :: Selector
selector = fold selectorChain


selectProfile :: MainOptions -> [Profile] -> FilePath -> Maybe Profile
selectProfile = unSelector selector
