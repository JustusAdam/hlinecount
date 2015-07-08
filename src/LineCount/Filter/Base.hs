module LineCount.Filter.Base where


import           LineCount.Base
import           LineCount.Profile
import           System.FilePath
import           Data.Foldable
import           Data.Composition
import           Data.List


{-|
  Represents a single step in the filter chain for files.
  Filters are '&&' chained, thus if one of them rejects the filename it is not scanned.
-}
newtype FileFilter = FileFilter { unFilter :: MainOptions -> [Profile] -> FilePath -> Bool }


instance Monoid FileFilter where
  mempty  = FileFilter (\_ _ _ -> True)
  mappend (FileFilter func1) (FileFilter func2) = FileFilter newfunc
    where
      newfunc a b c = func1 a b c && func2 a b c
