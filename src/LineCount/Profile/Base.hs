module LineCount.Profile.Base
    ( Profile(..)
    ) where


data Profile = Profile { canonicalName :: String
                       , fileExtensions   :: [FilePath]
                       , commentDelimiter :: [String]
                       , acceptedNames    :: [String]
                       } deriving (Show, Eq)


instance Monoid Profile where
  mempty = Profile mempty mempty mempty mempty
  mappend (Profile a1 b1 c1 d1) (Profile a2 b2 c2 d2) =
    Profile
      (a1 `mappend` ", " `mappend` a2)
      (b1 `mappend` b2)
      (c1 `mappend` c2)
      (d1 `mappend` d2)
