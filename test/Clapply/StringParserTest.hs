module Clapply.StringParserTest where

import Data.Either (isLeft)

import Clapply.Parser
import Clapply.StringParser
import Clapply.TestUtil
import Test.HUnit

suite = TestLabel "StringParser" (TestList
    [ pendTest
    , pcharTest
    , pdigitTest
    , pstringTest
    ])

pendTest = TestLabel "pend" (TestList
    [ TestCase $ assertEqual "empty" () (unsafeParse pend "")
    , TestCase $ assertBool "non-empty" (isLeft $ parse pend "a")
    ])

pcharTest = TestLabel "pchar" (TestList
    [ TestCase $ assertBool "empty" (isLeft $ eval (pchar 'a') "")
    , TestCase $ assertBool "mismatch" (isLeft $ eval (pchar 'a') "A")
    , TestCase $ assertEqual "match" ('a',"b") (unsafeRun (pchar 'a') "ab")
    ])

pdigitTest = TestLabel "pdigit" (TestList 
    [ TestCase $ assertEqual "digit" ('1',"") (unsafeRun pdigit "1")
    , TestCase $ assertBool "non-digit" (isLeft $ parse pdigit "a")
    ])

pstringTest = TestLabel "pstring" (TestList
    [ TestCase $ assertEqual "match" ("abc","def") (unsafeRun (pstring "abc") "abcdef")
    , TestCase $ assertBool "mismatch" (isLeft $ parse (pstring "abc") "def")
    , TestCase $ assertBool "partial match" (isLeft $ parse (pstring "abc") "ab")
    ])
