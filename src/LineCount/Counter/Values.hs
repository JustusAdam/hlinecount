module LineCount.Counter.Values where


import           Control.Monad.State.Lazy
import           Control.Monad.Trans.Maybe
import           Data.Char
import           Data.Function.JAExtra
import           Data.List
import           LineCount.Base
import           LineCount.Counter.Base
import           LineCount.Profile


isEmpty :: String -> Bool
isEmpty = all isSpace


nonEmptyCounter :: Counter
nonEmptyCounter = Counter (const2 func)
  where
    func line
      | isEmpty line = mzero
      | otherwise    = return $ mempty { nonEmpty = 1 }


emptyCounter :: Counter
emptyCounter = Counter (const2 func)
  where
    func line
      | isEmpty line = return $ mempty { emptyLines = 1 }
      | otherwise    = mzero


singleLineCommentCounter :: Counter
singleLineCommentCounter = Counter (const func)
  where
    func (Profile { commentDelimiter = dl }) line
      | isComment = return $ mempty { commentLines = 1 }
      | otherwise = mzero
      where
        isComment = or $ sequenceA (map isPrefixOf dl) $ filter (not . isSpace) line


multiLineCommentCounter :: Counter
multiLineCommentCounter = Counter (const func)
  where
    func :: Profile -> String -> MaybeT (State CounterState) CalcResult
    func (Profile { multiLineCommentDelimiters = cd }) line = do
      cs@(CounterState { currentDelimiter = isInside }) <- get
      case isInside of
        Just (_, endDelim)
          | endDelim `isPrefixOf` truncated -> do
            put $ cs { currentDelimiter = mzero }
            increment
        Just _ -> increment
        Nothing ->
          case find (finder . fst) cd of
            Just delim -> do
              put $ cs { currentDelimiter = return delim }
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
