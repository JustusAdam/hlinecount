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
        handleFile
      where
        handleFile
          -- the return type of this function is 'IO a' to allow it to expand later and do IO as well, if necessary
          | isFileAllowed file = return $ return $ File file ()
          | otherwise          = return Nothing
        handleDirectory
          | recu      =
            (return . Directory file . catMaybes) <$> subtrees
          | otherwise = return Nothing
          where
            allowedSubNodes = filter isAllowed <$> getDirectoryContents file
            subtrees = allowedSubNodes >>= mapM (buildTree' . (file </>))


        isAllowed = not . notAllowed
          where
            notAllowed
              | hidden    = (||) <$> isIgnored <*> isPrefixOf "." . takeFileName
              | otherwise = (||) <$> isIgnored <*> flip elem [".", ".."]
              where
                isIgnored = or . sequenceA (map isSubsequenceOf ign)

        isFileAllowed = Filter.isAllowed opts chosenProfiles


scanDir :: MainOptions -> [Profile] -> [FilePath] -> IO CalcResult
scanDir opts profiles paths = do
  trees    <- catMaybes <$> mapM (buildTree opts profiles) paths
  measured <- mapM (measureTree opts profiles) trees
  return $ fold $ map fold measured


measureTree :: MainOptions -> [Profile] -> DirTree a -> IO (DirTree CalcResult)
measureTree opts profs (Directory name contents) = Directory name <$> mapM (measureTree opts profs) contents
measureTree opts profs (File name _) =
  File name . maybe (const mempty) (countAll opts) (lookup (takeExtension name) (prfsToAssocList profs)) . lines <$> readFile name
