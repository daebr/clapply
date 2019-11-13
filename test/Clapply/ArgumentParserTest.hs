module Clapply.ArgumentParserTest where

import Clapply.ArgumentParser
import Test.HUnit

suite = TestLabel "ArgumentParser" (TestList
    [ unitTest
    , integrationTest
    ])

unitTest = TestLabel "unit" (TestList
    [ ])

integrationTest = TestLabel "integration" (TestList
    [ ])
