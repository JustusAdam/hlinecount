module LineCount.Filter
  ( isAllowed
  , hiddenFilter
  , sameFolderFilter
  , optsFilter
  , FileFilter(..)
  ) where


import           LineCount.Base
import           LineCount.Profile
import           System.FilePath
import           Data.List
import           Data.Foldable
import           Data.Char
import           Data.Composition


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


hiddenFilter :: FileFilter
hiddenFilter = FileFilter func
  where
    func (MainOptions { ignoreHidden = hidden }) _ f
      | hidden    = not (isPrefixOf "." $ takeFileName f)
      | otherwise = True


sameFolderFilter :: FileFilter
sameFolderFilter = FileFilter func
  where
    func _ _ = flip notElem [".", ".."] . takeFileName


optsFilter :: FileFilter
optsFilter = FileFilter func
  where
    func (MainOptions { ignorePaths = ip}) _ f =
      and $ sequenceA (map (not .: isSubsequenceOf) ip) f


filterChain :: [FileFilter]
filterChain =
  [ optsFilter
  , sameFolderFilter
  , hiddenFilter
  ]


fileFilter :: FileFilter
fileFilter = fold filterChain


isAllowed :: MainOptions -> [Profile] -> FilePath -> Bool
isAllowed = unFilter fileFilter
