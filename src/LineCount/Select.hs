module LineCount.Select
  ( Selector(..)
  , defaultSelector
  ) where

import LineCount.Profile.Base
import LineCount.Base
import System.FilePath
import Control.Monad
import Data.Foldable


{-|
  Represents a step int the 'Profile' selection process. Results are 'mplus' chained,
  ergo from left to right the function to first return a 'Just' value selects the profile.
-}
newtype Selector = Selector (MainOptions -> [Profile] -> FilePath -> Maybe Profile)


instance Monoid Selector where
  mempty = Selector (\_ _ _ -> Nothing)
  mappend (Selector s1) (Selector s2) = Selector newfunc
    where
      newfunc opts profs path = s1 opts profs path `mplus` s2 opts profs path


defaultSelector :: Selector
defaultSelector = Selector func
  where
    func _ profiles path = lookup (takeExtension path) $ prfsToAssocList profiles


selectorChain :: [Selector]
selectorChain =
  [ defaultSelector
  ]


selector :: Selector
selector = fold selectorChain
