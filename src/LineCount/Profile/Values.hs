{-# LANGUAGE UnicodeSyntax #-}
module LineCount.Profile.Values
    ( providedProfiles
    , profiles
    ) where


import qualified Data.Map.Lazy          as Map
import           Data.Monoid.Unicode
import           LineCount.Profile.Base



profileHaskell ∷ Profile
profileHaskell = (∅) { canonicalName    = "Haskell"
                     , fileExtensions   = [".hs", ".lhs"]
                     , commentDelimiter = ["--"]
                     , acceptedNames    = ["haskell", "hs"]
                     }


profilePython ∷ Profile
profilePython = (∅) { canonicalName    = "Python"
                    , fileExtensions   = [".py"]
                    , commentDelimiter = ["#"]
                    , acceptedNames    = ["python", "py"]
                    }


profileJava ∷ Profile
profileJava = (∅) { canonicalName    = "Java"
                  , fileExtensions   = [".java"]
                  , commentDelimiter = ["//"]
                  , acceptedNames    = ["java"]
                  }


profileC ∷ Profile
profileC = (∅) { canonicalName    = "C"
               , fileExtensions   = [".c", ".h"]
               , commentDelimiter = ["//"]
               , acceptedNames    = ["c"]
               }


profileCsharp ∷ Profile
profileCsharp = (∅) { canonicalName    = "C#"
                    , fileExtensions   = [".cs"]
                    , commentDelimiter = ["//"]
                    , acceptedNames    = ["csharp", "c#"]
                    }


profileCPP ∷ Profile
profileCPP = (∅) { canonicalName    = "C++"
                 , fileExtensions   = [".cpp", ".h", ".hpp", ".c++"]
                 , commentDelimiter = ["//"]
                 , acceptedNames    = ["cplusplus", "cpp", "c++"]
                 }


profileFsharp ∷ Profile
profileFsharp = (∅) { canonicalName    = "F#"
                    , fileExtensions   = [".fs", ".fsi"]
                    , commentDelimiter = ["--"]
                    , acceptedNames    = ["fsharp", "fs"]
                    }


profileElm ∷ Profile
profileElm = (∅) { canonicalName    = "Elm"
                 , fileExtensions   = [".elm"]
                 , commentDelimiter = ["--"]
                 , acceptedNames    = ["elm"]
                 }



__all ∷ [Profile]
__all =
  [ profileHaskell
  , profilePython
  , profileC
  , profileCPP
  , profileCsharp
  , profileFsharp
  , profileJava
  , profileElm
  ]


providedProfiles ∷ [String]
providedProfiles = map canonicalName __all


profiles ∷ Map.Map String Profile
profiles = Map.fromList $ prfsToAssocList __all
