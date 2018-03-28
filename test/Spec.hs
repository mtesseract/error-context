import qualified Control.Error.Context.Test
import           Test.Tasty


main :: IO ()
main = do
  defaultMain $
    testGroup "Test Suite"
    [ Control.Error.Context.Test.tests
    ]
