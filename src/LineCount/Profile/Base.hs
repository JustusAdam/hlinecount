module LineCount.Profile.Base
    ( Profile(..)
    ) where


data Profile = Profile { fileExtensions   :: [FilePath]
                       , commentDelimiter :: [String]
                       , acceptedNames    :: [String]
                       } deriving (Show, Eq)


instance Monoid Profile where
  mempty = Profile [] [] []
  mappend (Profile a1 b1 c1) (Profile a2 b2 c2) = Profile (a1 ++ a2) (b1 ++ b2) (c1 ++ c2)
