module Main where

import qualified Control.Error as E
import qualified Data.Sequence as Seq
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Relude
import qualified System.Environment as Env
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Hedgehog as Tasty

--TODO: nicer output
--TODO: consider script
--TODO: flags
main :: IO ()
main = do
  versions <- Env.getArgs
  case nonEmpty versions of
    Nothing -> error "Pass at least 1 version number argument"
    Just nonE -> do
      firstBad <- run id getAssessmentStdin nonE
      putStrLn ("First bad version is " <> firstBad)

run ::
  (a -> String) ->
  (a -> IO Assessment) ->
  NonEmpty a ->
  IO a
run showA getAssessment nonE = do
  let earliestBad = last nonE
  let others = Seq.fromList (init nonE)
  case binarySplit earliestBad others of
    Nothing -> pure earliestBad
    Just split -> do
      run' showA getAssessment split

run' ::
  (a -> String) ->
  (a -> IO Assessment) ->
  Split a ->
  IO a
run' showA getAssessment split@Split {..} = do
  print (showSplit showA split)
  assessment <- getAssessment split_focus
  case assess assessment split of
    Left (Exhausted a) -> pure a
    Right newSplit -> run' showA getAssessment newSplit

data Split a = Split
  { split_older :: Seq.Seq a,
    split_focus :: a,
    split_newer :: Seq.Seq a,
    split_earliestBad :: a
  }
  deriving stock (Eq)

searchFinished :: Split a -> Bool
searchFinished Split {..} = Seq.null split_older && Seq.null split_newer

showSplit :: (a -> String) -> Split a -> String
showSplit showA Split {..} =
  "[" <> older <> " > " <> focus <> " < " <> newer <> "], earliest bad: " <> earliestBad
  where
    focus = showA split_focus
    older = intercalate "," (showA <$> toList split_older)
    newer = intercalate "," (showA <$> toList split_newer)
    earliestBad = showA split_earliestBad

data Assessment
  = Assessment_Good
  | Assessment_Bad

getAssessmentStdin :: String -> IO Assessment
getAssessmentStdin focus = do
  putStrLn ("Current version is " <> focus <> ". good/bad?")
  str <- getLine
  case str of
    "good" -> pure Assessment_Good
    "bad" -> pure Assessment_Bad
    _ -> do
      putStrLn "Invalid selection. Try again."
      getAssessmentStdin focus

newtype Exhausted a = Exhausted a

assess :: Assessment -> Split a -> Either (Exhausted a) (Split a)
-- good means look for when it went wrong in newer versions
assess Assessment_Good Split {..} = E.note (Exhausted split_earliestBad) (binarySplit split_earliestBad split_newer)
-- bad means look for where it went wrong in older versions
assess Assessment_Bad Split {..} =
  let newBad = split_focus
   in E.note (Exhausted newBad) (binarySplit newBad split_older)

binarySplit :: a -> Seq.Seq a -> Maybe (Split a)
binarySplit earliestBad sq
  | Seq.null sq = Nothing
  | otherwise =
    let idx = ceiling (fromIntegral (Seq.length sq) / (2.0 :: Double))
     in case Seq.splitAt idx sq of
          (l, newer) -> case Seq.viewr l of
            Seq.EmptyR -> Nothing
            (older Seq.:> focus) ->
              Just
                Split
                  { split_older = older,
                    split_focus = focus,
                    split_newer = newer,
                    split_earliestBad = earliestBad
                  }

-------------------------------------------------------------------------------
testMain :: IO ()
testMain = Tasty.defaultMain tests

tests :: Tasty.TestTree
tests =
  Tasty.testGroup
    "bisector"
    [ Tasty.testProperty "finds the first bad" $
        HH.property $ do
          goods <- fmap TestValue_Good . toList <$> HH.forAll (Gen.set (Range.linear 0 20) genStr)
          bads <- fmap TestValue_Bad . toList <$> HH.forAll (Gen.set (Range.linear 1 20) genStr)
          let Just firstBad = E.headMay bads
          let Just nonE = nonEmpty (goods <> bads)
          res <- liftIO (run show (pure . checkGood) nonE)
          res HH.=== firstBad
    ]
  where
    genStr = Gen.string (Range.linear 0 20) Gen.ascii
    checkGood (TestValue_Good _) = Assessment_Good
    checkGood (TestValue_Bad _) = Assessment_Bad

data TestValue
  = TestValue_Good String
  | TestValue_Bad String
  deriving stock (Show, Eq, Ord)
