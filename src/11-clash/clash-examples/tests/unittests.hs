import Test.Tasty
import qualified Tests.Example.Project
import qualified Tests.FullAdder
import qualified Tests.FullAdderMultibits
import qualified Tests.Pairwise
import qualified Tests.SumReduce
import Prelude

main :: IO ()
main =
  defaultMain $
    testGroup
      "."
      [ Tests.Example.Project.accumTests
      , Tests.FullAdder.accumTests
      , Tests.FullAdderMultibits.accumTests
      , Tests.SumReduce.accumTests
      , Tests.Pairwise.accumTests
      ]
