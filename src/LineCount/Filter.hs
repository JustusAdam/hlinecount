module LineCount.Filter where


import           LineCount.Base
import           LineCount.Profile
import           System.FilePath
import           Data.List


{-|
  Represents a single step in the filter chain for files.
  Filters are '&&' chained, thus if one of them rejects the filename it is not scanned.
-}
newtype FileFilter = FileFilter (MainOptions -> [Profile] -> String -> Bool)


instance Monoid FileFilter where
  mempty  = FileFilter (\_ _ _ -> True)
  mappend (FileFilter func1) (FileFilter func2) = FileFilter newfunc
    where
      newfunc a b c = func1 a b c && func2 a b c


hiddenFilter :: FileFilter
hiddenFilter = FileFilter func
  where
    func (MainOptions { ignoreHidden = hidden }) _
      | hidden    = not . isPrefixOf "." . takeFileName
      | otherwise = const True


sameFolderFilter :: FileFilter
sameFolderFilter = FileFilter func
  where
    func _ _ = not . flip elem [".", ".."] . takeFileName


{-|
  Represents a single step in the filter chain for the entire code.
  (This is to allow for multi line comments to be detected by this filter.)
  The chaining operation is function composition, the result of one filter is passed onto the next.
-}
newtype LineFilter = LineFilter (Profile -> [String] -> [String])


instance Monoid LineFilter where
  mempty = LineFilter (\_ b -> b)
  mappend (LineFilter func1) (LineFilter func2) = LineFilter newfunc
    where
      newfunc prof = func2 prof . func1 prof
