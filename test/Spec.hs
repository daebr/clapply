import Data.Functor (void)
import Test.HUnit (Test(..), runTestTT)

import qualified Clapply.ParserTest as Parser

suite :: Test
suite = TestLabel "clapply" (TestList
    [ Parser.suite
    ])

main :: IO ()
main = void $ runTestTT suite
