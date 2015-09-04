{-# LANGUAGE UnicodeSyntax #-}
module LineCount.Filter.Base where


import           Control.Arrow
import           Data.Function
import           Data.Function.JAExtra
import           Data.Tuple.JAExtra
import           LineCount.Base
import           LineCount.Profile
import           Prelude.Unicode


{-|
  Represents a single step in the filter chain for files.
  Filters are '&&' chained, thus if one of them rejects the filename it is not scanned.
-}
newtype FileFilter = FileFilter { unFilter ∷ MainOptions → [Profile] → FilePath → Bool }


instance Monoid FileFilter where
  mempty  = FileFilter (const3 True)
  mappend (FileFilter func1) (FileFilter func2) = FileFilter newfunc
    where
      newfunc = curry3 (uncurry (&&) ∘ ((&&&) `on` uncurry3) func1 func2)
