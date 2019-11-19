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
    , optionTest
    ])

textTest = TestLabel "text" (TestList
    [ TestCase $ assertBool "empty" (isLeft $ parse text [])
    , TestCase $ assertEqual "non-empty" "abc" (unsafeParse text ["abc"])
    ])

switchTest = TestLabel "switch" (TestList
    [ TestCase $ assertEqual "short" (Just ()) (unsafeParse (switch ["-s","--switch"]) ["-s"]) 
    , TestCase $ assertEqual "long" (Just ()) (unsafeParse (switch ["-s","--switch"]) ["--switch"])
    , TestCase $ assertEqual "none" Nothing (unsafeParse (switch ["-s","--switch"]) [])
    ])

optionTest = TestLabel "option" (TestList
    [ TestCase $ assertEqual "short" ("my-value", []) (unsafeRun (option ["-o", "--option"]) ["-o", "my-value"])
    , TestCase $ assertEqual "long" ("my-value", []) (unsafeRun (option ["-o", "--option"]) ["--option", "my-value"])
    , TestCase $ assertBool "none" (isLeft $ parse (option ["-o", "--option"]) ["-k", "value"])
    ])

integrationTest = TestLabel "integration" (TestList
    [
    ])
