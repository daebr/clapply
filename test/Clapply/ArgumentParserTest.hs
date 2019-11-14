module Clapply.ArgumentParserTest where

import Clapply.ArgumentParser
import Test.HUnit

suite = TestLabel "ArgumentParser" (TestList
    [ unitTest
    , integrationTest
    ])

unitTest = TestLabel "unit" (TestList
    [ flagTest
    ])

flagTest = TestLabel "flag" (TestList
    [ -- TestCase $ assertEqual "short" True (unsafeParse (pflag "f" "flag") ["-f"]) 
    -- ,-TestCase $ assertEqual "long" True (unsafeParse (pflag "f" "flag") ["--flag"])
    -- , TestCase $ assertEqual "none" False (unsafeParse (pflag "f", "flag") [])
    ])

integrationTest = TestLabel "integration" (TestList
    [
    ])
