module LineCount.Counter where


import Control.Monad.State.Lazy
import Control.Monad.Trans.Maybe
import LineCount.Base
import LineCount.Profile
import Data.Monoid
import Data.Char
import Data.List


data CounterState = CounterState { currentDelimiter :: Maybe (String, String) } deriving (Eq, Show)


newtype Counter = Counter (MainOptions -> Profile -> String -> MaybeT (State CounterState) CalcResult)


emptyCounter :: Counter
emptyCounter = Counter func
  where
    func _ _ line
      | isEmpty   = mzero
      | otherwise = return $ mempty { emptyLines = 1 }
      where
        isEmpty = any (not . isSpace) line


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
    func _ (Profile { multiLineCommentDelimiters = cd }) line = do
      cs@(CounterState { currentDelimiter = isInside }) <- get
      case isInside of
        Just (_, endDelim) ->
          if endDelim `isPrefixOf` truncated
            then do
              put $ cs { currentDelimiter = Nothing }
              return $ mempty { commentLines = 1 }
            else
              return $ mempty { commentLines = 1 }
        Nothing ->
          case find (finder . fst) cd of
            Just delim -> do
              put $ cs { currentDelimiter = Just delim }
              return $ mempty { commentLines = 1 }
            Nothing -> mzero
      where
        truncated = dropWhile isSpace line
        finder = flip isPrefixOf truncated