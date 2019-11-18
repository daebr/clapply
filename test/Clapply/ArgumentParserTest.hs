module Clapply.ArgumentParserTest where

import Clapply.ArgumentParser
import Clapply.TestUtil
import Test.HUnit

suite = TestLabel "ArgumentParser" (TestList
    [ unitTest
    , integrationTest
    , switchTest
    ])

unitTest = TestLabel "unit" (TestList
    [ pallTest
    ])

pallTest = TestLabel "pall" (TestList
    [ TestCase $ assertEqual "success" ("my-value", []) (unsafeRun pall ["my-value"])
    ])

switchTest = TestLabel "flag" (TestList
    [ TestCase $ assertEqual "short" (Just ()) (unsafeParse (switch ["-s","--switch"]) ["-s"]) 
    , TestCase $ assertEqual "long" (Just ()) (unsafeParse (switch ["-s","--switch"]) ["--switch"])
    , TestCase $ assertEqual "none" Nothing (unsafeParse (switch ["-s","--switch"]) [])
    ])

integrationTest = TestLabel "integration" (TestList
    [
    ])
