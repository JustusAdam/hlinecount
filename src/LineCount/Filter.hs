module LineCount.Filter
  ( FileFilter(..)
  , fileFilter
  ) where


import           LineCount.Base
import           LineCount.Profile
import           System.FilePath
import           Data.List
import           Data.Foldable
import           Data.Char


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


fileFilterChain :: [FileFilter]
fileFilterChain =
  [ sameFolderFilter
  , hiddenFilter
  ]


fileFilter :: FileFilter
fileFilter = fold fileFilterChain
