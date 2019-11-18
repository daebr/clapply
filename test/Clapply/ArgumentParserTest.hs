module Clapply.ArgumentParserTest where

import Data.Either (isLeft)
import Clapply.ArgumentParser
import Clapply.TestUtil
import Test.HUnit

suite = TestLabel "ArgumentParser" (TestList
    [ unitTest
    , integrationTest
    ])

unitTest = TestLabel "unit" (TestList
    [ textTest
    , switchTest
    ])

textTest = TestLabel "text" (TestList
    [ TestCase $ assertBool "empty" (isLeft $ parse text [])
    , TestCase $ assertEqual "non-empty" "abc" (unsafeParse text ["abc"])
    ])

switchTest = TestLabel "flag" (TestList
    [ TestCase $ assertEqual "short" (Just ()) (unsafeParse (switch ["-s","--switch"]) ["-s"]) 
    , TestCase $ assertEqual "long" (Just ()) (unsafeParse (switch ["-s","--switch"]) ["--switch"])
    , TestCase $ assertEqual "none" Nothing (unsafeParse (switch ["-s","--switch"]) [])
    ])

integrationTest = TestLabel "integration" (TestList
    [
    ])
