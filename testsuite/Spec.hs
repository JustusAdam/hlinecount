module Spec where


import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)


import LineCount.Profile
import LineCount.Counter
import LineCount
import Control.Monad.Trans.Maybe
import Control.Monad.State.Lazy


emptyOpts = MainOptions True [] [] True [] []


emptyProfile = Profile "" [] [] [] []


testACounter :: Counter -> MainOptions -> Profile -> CounterState -> String -> Maybe CalcResult
testACounter c opts profile state input = runMaybeT (unCounter c opts profile input) `evalState` state


testACounterWEmpty :: Counter -> String -> Maybe CalcResult
testACounterWEmpty c = testACounter c emptyOpts emptyProfile emptyCS


main :: IO ()
main = hspec $ do
  countersSpec


countersSpec :: Spec
countersSpec = do
  let codeline = Just (mempty { nonEmpty = 1 })
  let commentLine = Just (mempty { commentLines = 1 })
  let emptyLine = Just (mempty { emptyLines = 1 })
  let reject = Nothing

  describe "emptyCounters" $ do
    let testFunc = testACounterWEmpty emptyCounter
    it "counts a line with only spaces" $
      testFunc "    " `shouldBe` emptyLine
    it "counts the empty string" $
      testFunc "" `shouldBe` emptyLine
    it "rejects a line containing a character" $
      testFunc "f" `shouldBe` reject
    it "rejects a line containing a number" $
      testFunc "4" `shouldBe` reject
    it "rejects a line containing spaces and a character" $
      testFunc "  f" `shouldBe` reject

  describe "nonEmptyCounter" $ do
    let testFunc = testACounterWEmpty nonEmptyCounter
    it "counts a single character as a new codeline" $
      testFunc "f" `shouldBe` codeline
    it "counts spaces followed by a character as new codeline" $
      testFunc "  f" `shouldBe` codeline
    it "counts a line with a non-word character" $
      testFunc "." `shouldBe` codeline
    it "rejects a line containing only spaces" $
      testFunc "   " `shouldBe` reject
    it "rejects the empty string" $
      testFunc "" `shouldBe` reject

  describe "singleLineCommentCounter" $ do
    let testFunc = testACounter singleLineCommentCounter emptyOpts (emptyProfile { commentDelimiter = ["#"] }) emptyCS
    it "counts a line with just the comment delimiter as new comment line" $
      testFunc "#" `shouldBe` commentLine
    it "counts a line with the delimiter and spaces before it" $
      testFunc "  #" `shouldBe` commentLine
    it "counts a line with other stuff and the delimiter as first character" $
      testFunc "  #erubv" `shouldBe` commentLine
    it "rejects a line not containing a comment delimiter" $
      testFunc " l" `shouldBe` reject
    it "rejects the empty string" $
      testFunc "" `shouldBe` reject
    it "rejects a line with the delimiter, but not as first character" $
      testFunc " j#sv" `shouldBe` reject

  describe "emptyCounter + nonEmptyCounter" $ do
    let testFunc = testACounterWEmpty (emptyCounter `mappend` nonEmptyCounter)
    -- expected result for a code line
    let clsucc = Just (mempty { nonEmpty = 1 })
    -- expected result for an empty line
    let elsucc = Just (mempty { emptyLines = 1 })
    it "counts a line with characters as a codeline" $
      testFunc "wjn" `shouldBe` codeline
    it "counts a line with spaces and characters as codeline" $
      testFunc "  erjgnv" `shouldBe` codeline
    it "counts a line with only spaces as empty line" $
      testFunc "  " `shouldBe` emptyLine
    it "counts the empty string as an empty line" $
      testFunc "" `shouldBe` emptyLine

  describe "emptyCounter + nonEmptyCounter + singleLineCommentCounter" $ do
    let testFunc = testACounter (singleLineCommentCounter `mappend` nonEmptyCounter `mappend` emptyCounter) emptyOpts (emptyProfile { commentDelimiter = ["#"] }) emptyCS
    it "counts a line with regular characters as codeline" $
      testFunc "wefjb" `shouldBe` codeline
    it "counts a line with no characters as empty" $
      testFunc "   " `shouldBe` emptyLine
    it "counts the empty string as empty line" $
      testFunc "" `shouldBe` emptyLine
    it "counts a line with the comment delimiter as comment line" $
      testFunc "#oernv" `shouldBe` commentLine
    it "counts a line with the delimiter in the middle as code" $
      testFunc "ejkn#ejnv" `shouldBe` codeline
