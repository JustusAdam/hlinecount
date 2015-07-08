module LineCount.Filter.Values where


import LineCount.Base
import LineCount.Profile
import LineCount.Filter.Base
import System.FilePath
import Data.Foldable
import Data.Composition
import Data.List


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
