module LineCount.Profile.Base
    ( Profile(..)
    ) where


data Profile = Profile { canonicalName    :: String  -- ^ this name will be shown in the tooltip
                       , fileExtensions   :: [FilePath]  -- ^ which files contain code of this language
                       , commentDelimiter :: [String]  -- ^ what denotes a single-line comment in this language
                       , acceptedNames    :: [String]  -- ^ under which names can this profile be found
                       } deriving (Show, Eq)


instance Monoid Profile where
  mempty = Profile mempty mempty mempty mempty
  mappend (Profile a1 b1 c1 d1) (Profile a2 b2 c2 d2) =
    Profile
      (a1 `mappend` ", " `mappend` a2)
      (b1 `mappend` b2)
      (c1 `mappend` c2)
      (d1 `mappend` d2)
