import Data.Functor (void)
import Test.HUnit (Test(..), runTestTT)

suite :: Test
suite = TestLabel "clapply" (TestList
    [
    ])

main :: IO ()
main = void $ runTestTT suite
