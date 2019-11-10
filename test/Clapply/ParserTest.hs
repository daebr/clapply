module Clapply.ParserTest where

import Data.Either (isLeft)

import Clapply.Parser
import Test.HUnit

suite = TestLabel "Paresr" (TestList
    [ pendTest 
    , pcharTest
    , pdigitTest
    , manyTest
    ])

pendTest = TestLabel "pend" (TestList
    [ TestCase $ assertEqual "empty" (Right (), "") $ parse pend ""
    , TestCase $ assertBool "non-empty" (isLeft . eval pend $ "a")
    ])

pcharTest = TestLabel "pchar" (TestList
    [ TestCase $ assertBool "empty" (isLeft . eval (pchar 'a') $ "")
    , TestCase $ assertBool "mismatch" (isLeft . eval (pchar 'a') $ "A")
    , TestCase $ assertEqual "match" (Right 'a', "b") (parse (pchar 'a') "ab")
    ])

pdigitTest = TestLabel "pdigit" (TestList [])

manyTest = TestLabel "many" (TestList 
    [ TestCase $ assertEqual "empty" (Right [], "") (parse (many $ pchar 'a') "")
    , TestCase $ assertEqual "mismatch" (Right [], "a") parse (many $ pchar 'A') "a")
    , TestCase $ assertEqual "one" (Right "a", "bc") (parse (many $ pchar 'a') "abc")
    , TestCase $ assertEqual "many" (Right "aaa", "bc") (parse (many $ pchar 'a') "aaabc")
    ])
