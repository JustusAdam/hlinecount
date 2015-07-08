module LineCount.Counter.Values where


import Control.Monad.State.Lazy
import Control.Monad.Trans.Maybe
import LineCount.Base
import LineCount.Profile
import Data.Char
import Data.List
import LineCount.Counter.Base


isEmpty :: String -> Bool
isEmpty = all isSpace


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
