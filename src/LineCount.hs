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


buildTree :: MainOptions -> FilePath -> IO (Maybe (DirTree ()))
buildTree
  (MainOptions
    { targetExtensions = exts
    , recursive = rec
    , ignorePaths = ign
    , ignoreHidden = hidden
    }) = buildTree'
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
          | rec       =
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

        isFileAllowed = (&&) <$> isAllowed <*> flip elem exts . takeExtension


scanDir :: MainOptions -> [FilePath] -> IO CalcResult
scanDir opts paths = do
  trees    <- catMaybes <$> mapM (buildTree opts) paths
  measured <- mapM measureTree trees
  return (
    let
      linecount = sum $ map sum measured
      filecount = sum $ map (foldl (const . (+ 1)) 0) trees
    in
      CalcResult  linecount filecount)


measureTree :: DirTree a -> IO (DirTree Int)
measureTree (Directory name contents) = Directory name <$> mapM measureTree contents
measureTree (File name _) =
  File name . countLines <$> readFile name
  where
    countLines    = length . nonEmptyLines
    nonEmptyLines = filter (not . isOnlyWhite) . lines
    isOnlyWhite   = not . any (not . isSpace)
