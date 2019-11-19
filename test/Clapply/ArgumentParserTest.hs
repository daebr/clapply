module Clapply.ArgumentParserTest where

import Data.Either (isLeft)
import Clapply.Prelude
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
    [ simpleOrderedTest
    ])

data SimpleOrdered = SimpleOrdered {
    target :: String,
    hasFlag :: Bool,
    info :: Maybe String
} deriving (Eq, Show)

flagParser = foldr (const2 True) False <$> switch ["-f","--flag"]
infoParser = optional $ option ["-i","--info"]
simpleOrderedParser = SimpleOrdered <$> text <*> flagParser <*> infoParser

simpleOrderedTest = TestLabel "simple ordered" (TestList
    [ TestCase $ assertEqual "target only" (SimpleOrdered "target" False Nothing) (unsafeParse simpleOrderedParser ["target"])
    , TestCase $ assertEqual "all fields" (SimpleOrdered "target" True (Just "info")) (unsafeParse simpleOrderedParser ["target","--flag","--info","info"])
    ])
