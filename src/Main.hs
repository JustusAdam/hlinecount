module Main where

import           Data.Bool        (bool)
import           Data.Composition
import           Data.List
import           Data.Maybe
import           Options
import           System.Directory
import           System.FilePath
import           System.IO
import           Control.Monad
import           Data.Foldable
import           LineCount
import           LineCount.Profile as P
import qualified Data.Map         as Map


recursiveDefault :: Bool
recursiveDefault = True
ignoredDefaultPaths :: [FilePath]
ignoredDefaultPaths =
  [ ".git/"
  , "build/"
  , "Setup.hs"
  ]
defaultFiles :: [String]
defaultFiles = []


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
          (\o -> o { optionShortFlags  = "f"
                   , optionLongFlags   = ["files"]
                   , optionDescription = "Fileextensions to include in the search."
                   , optionDefault     = defaultFiles
                   })
    <*> defineOption
          optionType_bool
          (\o -> o { optionLongFlags   = ["ignore-hidden"]
                   , optionDefault     = True
                   , optionDescription = "Ignore hidden files."
                   })
    <*> defineOption
          (optionType_list ',' optionType_string)
          (\o -> o { optionShortFlags  = "p"
                   , optionDefault     = []
                   , optionDescription = "Choose a predefined profile"
                   , optionLongFlags   = ["profile"]
                   })
    <*> defineOption
          (optionType_list ',' optionType_string)
          (\o -> o { optionLongFlags   = ["comment"]
                   , optionShortFlags  = "c"
                   , optionDescription = "Specify comment delimiters"
                   , optionDefault     = []
                   })


add :: CalcResult -> CalcResult -> CalcResult
add (CalcResult { lineCount = c1count }) c2 = c2 { lineCount = c1count }


integrateProfile :: MainOptions -> Profile -> MainOptions
integrateProfile
  m@(MainOptions { targetExtensions = t })
  p@(Profile { fileExtensions = fex })
  =
  m { targetExtensions = t `union` fex }


plur :: Int -> String
plur i
  | i < 2     = ""
  | otherwise = "s"


main :: IO ()
main = runCommand main'
  where
    main' :: MainOptions -> [FilePath] -> IO ()
    main' opts paths = do
      let chosenProfiles = mapMaybe (`Map.lookup` profiles) $ selProfiles opts
      let newOpts = integrateProfile opts $ mconcat chosenProfiles
      res@(CalcResult { lineCount = lk, fileCount = fk }) <- scanDir newOpts paths
      print newOpts
      putStrLn $ "Counted " ++ show lk ++ " line" ++ plur lk
      putStrLn $ "In " ++ show fk ++ " file" ++ plur fk
