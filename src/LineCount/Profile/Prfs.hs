module LineCount.Profile.Prfs
    ( __all
    ) where

import LineCount.Profile.Base


profileHaskell :: Profile
profileHaskell = mempty { canonicalName    = "Haskell"
                        , fileExtensions   = [".hs", ".lhs"]
                        , commentDelimiter = ["--"]
                        , acceptedNames    = ["haskell", "hs"]
                        }


profilePython :: Profile
profilePython = mempty { canonicalName    = "Python"
                       , fileExtensions   = [".py"]
                       , commentDelimiter = ["#"]
                       , acceptedNames    = ["python", "py"]
                       }


profileJava :: Profile
profileJava = mempty { canonicalName    = "Java"
                     , fileExtensions   = [".java"]
                     , commentDelimiter = ["//"]
                     , acceptedNames    = ["java"]
                     }


profileC :: Profile
profileC = mempty { canonicalName    = "C"
                  , fileExtensions   = [".c", ".h"]
                  , commentDelimiter = ["//"]
                  , acceptedNames    = ["c"]
                  }


profileCsharp :: Profile
profileCsharp = mempty { canonicalName    = "C#"
                       , fileExtensions   = [".cs"]
                       , commentDelimiter = ["//"]
                       , acceptedNames    = ["csharp", "c#"]
                       }


profileCPP :: Profile
profileCPP = mempty { canonicalName    = "C++"
                    , fileExtensions   = [".cpp", ".h", ".hpp", ".c++"]
                    , commentDelimiter = ["//"]
                    , acceptedNames    = ["cplusplus", "cpp", "c++"]
                    }


profileFsharp :: Profile
profileFsharp = mempty { canonicalName    = "F#"
                       , fileExtensions   = [".fs", ".fsi"]
                       , commentDelimiter = ["--"]
                       , acceptedNames    = ["fsharp", "fs"]
                       }


profileElm :: Profile
profileElm = mempty { canonicalName    = "Elm"
                    , fileExtensions   = [".elm"]
                    , commentDelimiter = ["--"]
                    , acceptedNames    = ["elm"]
                    }



__all :: [Profile]
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
