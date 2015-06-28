module LineCount.Profile.Base
    ( Profile(..)
    ) where


import Data.Monoid


data Profile = Profile { canonicalName    :: String  -- ^ this name will be shown in the tooltip
                       , fileExtensions   :: [FilePath]  -- ^ which files contain code of this language
                       , commentDelimiter :: [String]  -- ^ what denotes a single-line comment in this language
                       , acceptedNames    :: [String]  -- ^ under which names can this profile be found
                       } deriving (Show, Eq)


instance Monoid Profile where
  mempty = Profile mempty mempty mempty mempty
  mappend (Profile a1 b1 c1 d1) (Profile a2 b2 c2 d2) =
    Profile
      (a1 <> ", " <> a2)
      (b1 <> b2)
      (c1 <> c2)
      (d1 <> d2)
