module LineCount.Base
    ( DirTree(..)
    , CalcResult(..)
    , MainOptions(..)
    ) where


data CalcResult = CalcResult { lineCount :: Int
                             , fileCount :: Int
                             } deriving (Eq, Show)


data DirTree a
  = File String a
  | Directory String [DirTree a]
  deriving (Eq, Show)


data MainOptions = MainOptions { recursive         :: Bool
                               , ignorePaths       :: [FilePath]
                               , targetExtensions  :: [String]
                               , ignoreHidden      :: Bool
                               , selProfiles       :: [String]
                               , commentDelimiters :: [String]
                               } deriving (Eq, Show)


instance Foldable DirTree where
  foldMap f (File _ n) = f n
  foldMap f (Directory _ cont) = foldMap (foldMap f) cont

  foldr f b (File _ value) = f value b
  foldr f b (Directory _ contents) = foldr (flip $ foldr f) b contents


instance Monoid CalcResult where
  mempty = CalcResult 0 0
  mappend (CalcResult a1 b1) (CalcResult a2 b2) = CalcResult (a1 + a2) (b1 + b2)
