{-# LANGUAGE UnicodeSyntax  #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main where

import           Data.Char
import           Data.List
import qualified Data.Map          as Map
import           Data.Maybe
import           LineCount
import           LineCount.Profile as P
import           Options
import Data.Monoid.Unicode
import Data.Monoid
import Prelude.Unicode
import Data.List.Unicode


recursiveDefault ∷ Bool
recursiveDefault = True
ignoredDefaultPaths ∷ [FilePath]
ignoredDefaultPaths =
  [ ".git/"
  , "build/"
  , "Setup.hs"
  ]
defaultFiles ∷ [String]
defaultFiles = (∅)


instance Options MainOptions where
  defineOptions = MainOptions
    <$> defineOption
          optionType_bool
          (\o → o { optionLongFlags = ["recursive"]
                   , optionShortFlags = "r"
                   , optionDefault = recursiveDefault
                   , optionDescription = "Scan directories recursively."
                   })
    <*> defineOption
          (optionType_list ',' optionType_string)
          (\o → o { optionShortFlags = "i"
                   , optionLongFlags = ["ignore"]
                   , optionDescription = "Ignore these paths."
                   , optionDefault = ignoredDefaultPaths
                   })
    <*> defineOption
          (optionType_list ',' optionType_string)
          (\o → o { optionShortFlags  = "f"
                   , optionLongFlags   = ["files"]
                   , optionDescription = "Fileextensions to include in the search."
                   , optionDefault     = defaultFiles
                   })
    <*> defineOption
          optionType_bool
          (\o → o { optionLongFlags   = ["ignore-hidden"]
                   , optionDefault     = True
                   , optionDescription = "Ignore hidden files."
                   })
    <*> defineOption
          (optionType_list ',' optionType_string)
          (\o → o { optionShortFlags  = "p"
                   , optionDefault     = []
                   , optionDescription =
                     "Select predefined profiles\n\
                     \    Available profiles are:\n    "
                     ⊕ intercalate ", " P.providedProfiles
                   , optionLongFlags   = ["profile"]
                   })
    <*> defineOption
          (optionType_list ',' optionType_string)
          (\o → o { optionLongFlags   = ["comment"]
                   , optionShortFlags  = "c"
                   , optionDescription = "Specify comment delimiters"
                   , optionDefault     = []
                   })


integrateProfile ∷ MainOptions → Profile → MainOptions
integrateProfile
  m@(MainOptions { targetExtensions = t })
  (Profile { fileExtensions })
  =
  m { targetExtensions = t ∪ fileExtensions }


plur ∷ Int → String
plur i
  | i < 2     = ""
  | otherwise = "s"


main ∷ IO ()
main = runCommand main'
  where
    main' ∷ MainOptions → [FilePath] → IO ()
    main' opts paths = do
      let chosenProfiles = mapMaybe (flip Map.lookup profiles ∘ map toLower) $ selProfiles opts
      print chosenProfiles
      (CalcResult { nonEmpty, fileCount }) ← scanDir opts chosenProfiles paths
      let nonEmpty' = getSum nonEmpty
          fileCount' = getSum fileCount
      putStrLn $ "Counted " ⊕ show nonEmpty' ⊕ " line" ⊕ plur nonEmpty'
      putStrLn $ "In " ⊕ show fileCount' ⊕ " file" ⊕ plur fileCount'
