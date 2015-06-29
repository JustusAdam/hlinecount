module LineCount.Counter (countAll) where


import Control.Monad.State.Lazy
import Control.Monad.Trans.Maybe
import LineCount.Base
import LineCount.Profile
import Data.Char
import Data.List
import Data.Maybe
import Data.Foldable


data CounterState = CounterState { currentDelimiter :: Maybe (String, String) } deriving (Eq, Show)


emptyCS :: CounterState
emptyCS = CounterState { currentDelimiter = Nothing }


newtype Counter = Counter { unCounter :: MainOptions -> Profile -> String -> MaybeT (State CounterState) CalcResult }


isEmpty :: String -> Bool
isEmpty = any (not . isSpace)


nonEmptyCounter :: Counter
nonEmptyCounter = Counter func
  where
    func _ _ line
      | isEmpty line = mzero
      | otherwise    = return $ mempty { nonEmpty = 1 }


emptyCounter :: Counter
emptyCounter = Counter func
  where
    func _ _ line
      | isEmpty line = return $ mempty { emptyLines = 1 }
      | otherwise    = mzero


singleLineCommentCounter :: Counter
singleLineCommentCounter = Counter func
  where
    func _ (Profile { commentDelimiter = dl }) line
      | isComment = return $ mempty { commentLines = 1 }
      | otherwise = mzero
      where
        isComment = or $ sequenceA (map isPrefixOf dl) $ filter (not . isSpace) line


multiLineCommentCounter :: Counter
multiLineCommentCounter = Counter func
  where
    func :: MainOptions -> Profile -> String -> MaybeT (State CounterState) CalcResult
    func _ (Profile { multiLineCommentDelimiters = cd }) line = do
      cs@(CounterState { currentDelimiter = isInside }) <- get
      case isInside of
        Just (_, endDelim) ->
          if endDelim `isPrefixOf` truncated
            then do
              put $ cs { currentDelimiter = Nothing }
              increment
            else
              increment
        Nothing ->
          case find (finder . fst) cd of
            Just delim -> do
              put $ cs { currentDelimiter = Just delim }
              increment
            Nothing -> mzero
      where
        increment = return $ mempty { commentLines = 1 }
        truncated = dropWhile isSpace line
        finder    = flip isPrefixOf truncated


counterChain :: [Counter]
counterChain =
  [ multiLineCommentCounter
  , singleLineCommentCounter
  , nonEmptyCounter
  , emptyCounter
  ]

countLine :: MainOptions -> Profile -> String -> State CounterState CalcResult
countLine opts prof =
  fmap (fromMaybe mempty) . runMaybeT . msum . combinedFunction
  where
    combinedFunction = sequenceA (map ((\f -> f opts prof) . unCounter) counterChain)


countAll :: MainOptions -> Profile -> [String] -> CalcResult
countAll opts profs = fold . flip evalState emptyCS . mapM (countLine opts profs)
