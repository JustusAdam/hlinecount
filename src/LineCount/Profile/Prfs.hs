module LineCount.Profile.Prfs
    ( __all
    ) where

import LineCount.Profile.Base


profileHaskell :: Profile
profileHaskell = Profile { fileExtensions   = [".hs", ".lhs"]
                         , commentDelimiter = ["--"]
                         , acceptedNames    = ["haskell", "hs"]
                         }


profilePython :: Profile
profilePython = Profile { fileExtensions   = [".py"]
                        , commentDelimiter = ["#"]
                        , acceptedNames    = ["python", "py"]
                        }

profileJava :: Profile
profileJava = Profile { fileExtensions = ["java"]
                      , commentDelimiter = ["//"]
                      , acceptedNames = ["java"]
                      }


__all :: [Profile]
__all =
  [ profileHaskell
  , profilePython
  , profileJava
  ]
