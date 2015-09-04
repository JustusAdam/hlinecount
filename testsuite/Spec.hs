module Spec where


import           Control.Monad.State.Lazy
import           Control.Monad.Trans.Maybe
import           LineCount
import           LineCount.Counter
import           LineCount.Counter.Base
import           LineCount.Counter.Values
import           LineCount.Filter.Base
import           LineCount.Filter.Values
import           LineCount.Profile
import           Test.Hspec


emptyOpts = MainOptions True [] [] True [] []


emptyProfile = mempty


testAFilter :: FileFilter -> MainOptions -> [Profile] -> FilePath -> Bool
testAFilter = unFilter


testAFilterWithEmpty :: FileFilter -> FilePath -> Bool
testAFilterWithEmpty f = testAFilter f emptyOpts []


filtersSpec :: Spec
filtersSpec = do

  describe "[function] hiddenFilter" $ do

    let testFunc = testAFilterWithEmpty hiddenFilter

    it "rejects \".\"" $
      testFunc "." `shouldBe` False
    it "rejects .⍵" $
      testFunc ".hello" `shouldBe` False
    it "accepts { a⍵ | a ≠ '.' }" $
      testFunc "hello" `shouldBe` True
    it "accepts 𝜖" $
      testFunc "" `shouldBe` True
    it "accepts { a⍵ | a ∈ SYMBOLS\\{'.'} }" $
      testFunc "," `shouldBe` True
    it "rejects { ⍵/. } " $
      testFunc "/." `shouldBe` False
    it "rejects a unix filepath with the last component ∈ { .⍵ }" $
      testFunc "some/path/.file" `shouldBe` False

  describe "[function] sameFolderFilter"  $ do

    let testFunc = testAFilterWithEmpty sameFolderFilter

    it "rejects \".\"" $
      testFunc "." `shouldBe` False
    it "rejects \"..\"" $
      testFunc ".." `shouldBe` False
    it "rejects an arbitrary unix path ending in \"/.\"" $
      testFunc "some/path/." `shouldBe` False
    it "rejects an arbitrary unix path ending in \"/..\"" $
      testFunc "some/path/.." `shouldBe` False
    it "accepts ⍵" $
      testFunc "word" `shouldBe` True
    it "accepts .⍵" $
      testFunc ".some" `shouldBe` True
    it "accepts an arbitrary path" $
      testFunc "/some/path" `shouldBe` True
    it "accepts an arbitrary path ending with a word prefixed with \".\"" $
      testFunc "/some/path/.file" `shouldBe` True


testACounter :: Counter -> MainOptions -> Profile -> CounterState -> String -> Maybe CalcResult
testACounter c opts profile state input = runMaybeT (unCounter c opts profile input) `evalState` state


testACounterWEmpty :: Counter -> String -> Maybe CalcResult
testACounterWEmpty c = testACounter c emptyOpts emptyProfile emptyCS


countersSpec :: Spec
countersSpec = do

  let codeline    = Just (mempty { nonEmpty = 1 })
  let commentLine = Just (mempty { commentLines = 1 })
  let emptyLine   = Just (mempty { emptyLines = 1 })
  let reject      = Nothing

  describe "[function] emptyCounters" $ do

    let testFunc = testACounterWEmpty emptyCounter

    it "counts {' '}*" $
      testFunc "    " `shouldBe` emptyLine
    it "counts 𝜖" $
      testFunc "" `shouldBe` emptyLine
    it "rejects { ⍵ | ⍵ ∉ {' '}* }" $
      testFunc "f" `shouldBe` reject
    it "rejects { ⍵iσ | ⍵, σ ∈ Σ*,  i ∈ {0-9} } " $
      testFunc "4" `shouldBe` reject
    it "rejects a line containing spaces and a character" $
      testFunc "  f" `shouldBe` reject

  describe "[function] nonEmptyCounter" $ do

    let testFunc = testACounterWEmpty nonEmptyCounter

    it "counts { ⍵ | ⍵ ∈ Σ\\{' '} } as a new codeline" $
      testFunc "f" `shouldBe` codeline
    it "counts { ⍵a | ⍵ ∈ {' '}*, a ∈ Σ\\{' '} } as new codeline" $
      testFunc "  f" `shouldBe` codeline
    it "counts a line with a non-word character" $
      testFunc "." `shouldBe` codeline
    it "rejects a line containing only spaces" $
      testFunc "   " `shouldBe` reject
    it "rejects the empty string" $
      testFunc "" `shouldBe` reject

  describe "[function] singleLineCommentCounter" $ do

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

  describe "[composed counter] emptyCounter + nonEmptyCounter" $ do

    let testFunc = testACounterWEmpty (emptyCounter `mappend` nonEmptyCounter)

    it "counts a line with characters as a codeline" $
      testFunc "wjn" `shouldBe` codeline
    it "counts a line with spaces and characters as codeline" $
      testFunc "  erjgnv" `shouldBe` codeline
    it "counts a line with only spaces as empty line" $
      testFunc "  " `shouldBe` emptyLine
    it "counts the empty string as an empty line" $
      testFunc "" `shouldBe` emptyLine

  describe "[composed counter] emptyCounter + nonEmptyCounter + singleLineCommentCounter" $ do

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


main :: IO ()
main = hspec $ do
  countersSpec
  filtersSpec
