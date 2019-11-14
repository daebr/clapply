import Data.Functor (void)
import Test.HUnit (Test(..), runTestTT)

import qualified Clapply.ParserTest as Parser
import qualified Clapply.StringParserTest as StringParser
import qualified Clapply.ArgumentParserTest as ArgumentParser

suite :: Test
suite = TestLabel "clapply" (TestList
    [ Parser.suite
    , StringParser.suite
    , ArgumentParser.suite
    ])

main :: IO ()
main = void $ runTestTT suite
