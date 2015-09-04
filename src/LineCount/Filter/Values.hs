{-# LANGUAGE UnicodeSyntax #-}
module LineCount.Filter.Values where


import           Data.Composition
import           Data.Foldable
import           Data.Function.JAExtra
import           Data.List
import           LineCount.Base
import           LineCount.Filter.Base
import           System.FilePath


hiddenFilter ∷ FileFilter
hiddenFilter = FileFilter func
  where
    func (MainOptions { ignoreHidden = hidden })
      | hidden    = const $ not . isPrefixOf "." . takeFileName
      | otherwise = const2 True


sameFolderFilter ∷ FileFilter
sameFolderFilter = FileFilter func
  where
    func = const2 $ flip notElem [".", ".."] . takeFileName


optsFilter ∷ FileFilter
optsFilter = FileFilter func
  where
    func (MainOptions { ignorePaths = ip}) =
      const $ and . sequenceA (map (not .: isSubsequenceOf) ip)


filterChain ∷ [FileFilter]
filterChain =
  [ optsFilter
  , sameFolderFilter
  , hiddenFilter
  ]


fileFilter ∷ FileFilter
fileFilter = fold filterChain
