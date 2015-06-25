module Main where

import           Data.Bool        (bool)
import           Data.Composition
import           Data.List
import           Data.Maybe       (catMaybes)
import           Options
import           System.Directory
import           System.FilePath
import           System.IO
import           Control.Monad    (void)
import           Data.Foldable


recursiveDefault :: Bool
recursiveDefault = True
ignoredDefaultPaths :: [FilePath]
ignoredDefaultPaths =
  [ ".git/"
  , "build/"
  , "Setup.hs"
  ]
defaultFiles :: [String]
defaultFiles =
  [ ".hs"
  , ".lhs"
  ]


data CalcResult = CalcResult { lineCount :: Int
                             , fileCount :: Int
                             } deriving (Eq, Show)


data MainOptions = MainOptions { recursive        :: Bool
                               , ignorePaths      :: [FilePath]
                               , targetExtensions :: [String]
                               , ignoreHidden     :: Bool
                               } deriving (Eq, Show)


data DirTree a
  = File String a
  | Directory String [DirTree a]
  deriving (Eq, Show)


instance Foldable DirTree where
  foldMap f (File _ n) = f n
  foldMap f (Directory _ cont) = foldMap (foldMap f) cont

  foldr f b (File _ value) = f value b
  foldr f b (Directory _ contents) = foldr (flip $ foldr f) b contents


instance Options MainOptions where
  defineOptions = MainOptions
    <$> defineOption
          optionType_bool
          (\o -> o { optionLongFlags = ["recursive"]
                   , optionShortFlags = "r"
                   , optionDefault = recursiveDefault
                   , optionDescription = "Scan directories recursively."
                   })
    <*> defineOption
          (optionType_list ',' optionType_string)
          (\o -> o { optionShortFlags = "i"
                   , optionLongFlags = ["ignore"]
                   , optionDescription = "Ignore these paths."
                   , optionDefault = ignoredDefaultPaths
                   })
    <*> defineOption
          (optionType_list ',' optionType_string)
          (\o -> o { optionShortFlags = "f"
                   , optionLongFlags = ["files"]
                   , optionDescription = "Fileextensions to include in the search."
                   , optionDefault = defaultFiles
                   })
    <*> defineOption
          optionType_bool
          (\o -> o { optionLongFlags = ["ignore-hidden"]
                   , optionDefault = True
                   , optionDescription = "Ignore hidden files."
                   })


add :: CalcResult -> CalcResult -> CalcResult
add (CalcResult { lineCount = c1count }) c2 = c2 { lineCount = c1count }


scanDir :: MainOptions -> [FilePath] -> IO CalcResult
scanDir opts@(MainOptions {
              targetExtensions = exts,
              recursive = recursive,
              ignorePaths = ign,
              ignoreHidden = hidden
              }) paths = do
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
  File name . lineCount <$> readFile name
  where
    lineCount     = length . nonEmptyLines
    nonEmptyLines = filter (not . isOnlyWhite) . lines
    isOnlyWhite   = not . any (not . isSpace)
    isSpace       = (==) ' '


buildTree :: MainOptions -> FilePath -> IO (Maybe (DirTree ()))
buildTree (MainOptions {
              targetExtensions = exts,
              recursive = recursive,
              ignorePaths = ign,
              ignoreHidden = hidden
              }) = buildTree'
  where
    buildTree' file =
      doesFileExist file >>=
      bool
        (doesDirectoryExist file >>= bool (return Nothing) handleDirectory)
        handleFile
      where
        handleFile
          | isFileAllowed file = return $ return $ File file ()
          | otherwise          = return Nothing
        handleDirectory
          | recursive =
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
        isDirAllowed = isAllowed


main :: IO ()
main = runCommand main'
  where
    main' :: MainOptions -> [FilePath] -> IO ()
    main' opts paths = scanDir opts paths >>= print
