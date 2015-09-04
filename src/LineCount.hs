{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UnicodeSyntax  #-}

module LineCount
  ( DirTree(..)
  , CalcResult(..)
  , scanDir
  , measureTree
  , buildTree
  , MainOptions(..)
  ) where


import           Control.Monad
import           Control.Monad.Unicode
import           Data.Bool             (bool)
import           Data.Foldable
import           Data.Maybe
import           Data.Monoid.Unicode
import           LineCount.Base
import           LineCount.Counter
import qualified LineCount.Filter      as Filter
import           LineCount.Profile
import           Prelude.Unicode
import           System.Directory
import           System.FilePath


buildTree ∷ MainOptions → [Profile] → FilePath → IO (Maybe (DirTree ()))
buildTree
  opts@(MainOptions { recursive })
  chosenProfiles
  = buildTree'
  where
    buildTree' file =
      doesFileExist file ≫=
      bool
        (doesDirectoryExist file ≫= bool (return mzero) handleDirectory)
        (return $ return $ File file (∅))
      where
        handleDirectory
          | recursive =
            (return ∘ Directory file ∘ catMaybes) <$> subtrees
          | otherwise = return mzero
          where
            allowedSubNodes = filter isFileAllowed  <$> fmap (file </>) <$> getDirectoryContents file
            subtrees = allowedSubNodes ≫= traverse buildTree'

        isFileAllowed = Filter.isAllowed opts chosenProfiles


scanDir ∷ MainOptions → [Profile] → [FilePath] → IO CalcResult
scanDir opts selectedProfiles paths = do
  trees    ← catMaybes <$> traverse (buildTree opts selectedProfiles) paths
  print $ foldr (mappend ∘ flattenDT selectedProfiles) (∅) trees
  measured ← traverse (measureTree opts selectedProfiles) trees
  print measured
  return $ fold $ map fold measured


measureTree ∷ MainOptions → [Profile] → DirTree a → IO (DirTree CalcResult)
measureTree opts profs (Directory name contents) = Directory name <$> traverse (measureTree opts profs) contents
measureTree opts profs (File name _) =
  File name . maybe (const (∅)) (countAll opts) fileProfile ∘ lines <$> readFile name
  where
    fileProfile = lookup (takeExtension name) (prfsToAssocList profs)


flattenDT ∷ [Profile] → DirTree a → [String]
flattenDT p (File name _)
  | extensionAccepted name = return name
  | otherwise = (∅)
  where
    extensionAccepted fileName = any ((∈) (takeExtension fileName) ∘ fileExtensions) p
flattenDT p (Directory _ c) = foldr ((⊕) ∘ flattenDT p) (∅) c
