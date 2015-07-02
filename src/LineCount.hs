module LineCount
  ( DirTree(..)
  , CalcResult(..)
  , scanDir
  , measureTree
  , buildTree
  , MainOptions(..)
  ) where


import           LineCount.Base
import           Data.Maybe
import           Data.List
import           System.Directory
import           System.FilePath
import           Data.Bool        (bool)
import           Data.Char
import qualified LineCount.Filter as Filter
import           LineCount.Select
import           LineCount.Counter
import           LineCount.Profile
import           Data.Foldable
import Debug.Trace


buildTree :: MainOptions -> [Profile] -> FilePath -> IO (Maybe (DirTree ()))
buildTree
  opts@(MainOptions { targetExtensions = exts
                    , recursive = recu
                    , ignorePaths = ign
                    , ignoreHidden = hidden
                    }
  )
  chosenProfiles
  = buildTree'
  where
    buildTree' file =
      doesFileExist file >>=
      bool
        (doesDirectoryExist file >>= bool (return Nothing) handleDirectory)
        (return $ return $ File file ())
      where
        handleDirectory
          | recu      =
            (return . Directory file . catMaybes) <$> subtrees
          | otherwise = return Nothing
          where
            allowedSubNodes = filter isFileAllowed  <$> map (file </>) <$> getDirectoryContents file
            subtrees = allowedSubNodes >>= mapM buildTree'

        isFileAllowed = Filter.isAllowed opts chosenProfiles


scanDir :: MainOptions -> [Profile] -> [FilePath] -> IO CalcResult
scanDir opts profiles paths = do
  trees    <- catMaybes <$> mapM (buildTree opts profiles) paths
  print trees
  measured <- mapM (measureTree opts profiles) trees
  print measured
  return $ fold $ map fold measured


measureTree :: MainOptions -> [Profile] -> DirTree a -> IO (DirTree CalcResult)
measureTree opts profs (Directory name contents) = Directory name <$> mapM (measureTree opts profs) contents
measureTree opts profs (File name _) =
  File name . maybe (const mempty) (countAll opts) fileProfile . lines <$> readFile name
  where
    fileProfile = lookup (takeExtension name) (prfsToAssocList profs)
