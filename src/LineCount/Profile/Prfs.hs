module LineCount.Profile.Prfs
    ( __all
    ) where

import LineCount.Profile.Base


profileHaskell :: Profile
profileHaskell = Profile { canonicalName    = "Haskell"
                         , fileExtensions   = [".hs", ".lhs"]
                         , commentDelimiter = ["--"]
                         , acceptedNames    = ["haskell", "hs"]
                         }


profilePython :: Profile
profilePython = Profile { canonicalName    = "Python"
                        , fileExtensions   = [".py"]
                        , commentDelimiter = ["#"]
                        , acceptedNames    = ["python", "py"]
                        }

profileJava :: Profile
profileJava = Profile { canonicalName    = "Java"
                      , fileExtensions   = [".java"]
                      , commentDelimiter = ["//"]
                      , acceptedNames    = ["java"]
                      }


profileCsharp :: Profile
profileCsharp = Profile { canonicalName    = "C#"
                        , fileExtensions   = [".cs"]
                        , commentDelimiter = ["//"]
                        , acceptedNames    = ["csharp", "c#"]
                        }


__all :: [Profile]
__all =
  [ profileHaskell
  , profilePython
  , profileJava
  , profileCsharp
  ]
