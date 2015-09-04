module LineCount.Select
  ( selectProfile
  ) where

import           Control.Arrow
import           Control.Monad
import           Data.Foldable
import           Data.Function
import           Data.Function.JAExtra
import           Data.Tuple.JAExtra
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
      newfunc = curry3 (uncurry mplus . ((&&&) `on` uncurry3) s1 s2)


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
