module Clapply.ParserTest where

import Data.Either (isLeft)
import Control.Applicative ((<|>))

import Clapply.Parser
import Test.HUnit

suite = TestLabel "Paresr" (TestList
    [ runTest
    , evalTest
    , execTest
    , parseTest
    , pendTest 
    , pcharTest
    , pdigitTest
    , pstringTest
    , andThenTest
    , orElseTest
    , anyOfTest
    , mapErrorTest
    , manyTest
    , many1Test
    ])

runTest = TestLabel "parse" (TestList
    [ TestCase $ assertEqual "success" (Right ('a',"")) (run (pchar 'a') "a")
    , TestCase $ assertBool "failure" (isLeft $ run (pchar 'a') "b")
    ])

evalTest = TestLabel "eval" (TestList
    [ TestCase $ assertEqual "success" (Right 'a') (eval (pchar 'a') "a")
    , TestCase $ assertBool "failure" (isLeft $ eval (pchar 'a') "b")
    ])

execTest = TestLabel "exec" (TestList
    [ TestCase $ assertEqual "success" (Right "b") (exec (pchar 'a') "ab")
    , TestCase $ assertBool "failure" (isLeft $ eval (pchar 'a') "b")
    ])

parseTest = TestLabel "parse" (TestList
    [ TestCase $ assertEqual "success" (Right 'a') (parse (pchar 'a') "a")
    , TestCase $ assertBool "failure" (isLeft $ parse (pchar 'a') "b")
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

orElseTest = TestLabel "orElse" (TestList
    [ TestCase $ assertEqual "Right <||> Right" ("a","aa") (unsafeRun (pure <$> pchar 'a' <||> many (pchar 'a')) "aaa")
    , TestCase $ assertEqual "Right <||> Left" ('a',"") (unsafeRun (pchar 'a' <||> pchar 'b') "a")
    , TestCase $ assertEqual "Left <||> Right" ('b',"") (unsafeRun (pchar 'a' <||> pchar 'b') "b")
    , TestCase $ assertBool "Left <||> Left" (isLeft $ parse (pchar 'a' <||> pchar 'b') "c")
    ])

anyOfTest = TestLabel "anyOf" (TestList
    [ TestCase $ assertEqual "[Right,Right]" ("a","aa") (unsafeRun (anyOf [pure <$> pchar 'a', many (pchar 'a')]) "aaa")
    , TestCase $ assertBool "[Left, Left]" (isLeft $ parse (anyOf [pchar 'a', pchar 'b']) "c")
    , TestCase $ assertEqual "[Left, Right]" ('b',"") (unsafeRun (anyOf [pchar 'a', pchar 'b']) "b")
    ])

andThenTest = TestLabel "andThen" (TestList
    [ TestCase $ assertEqual "Right .&&. Right" (('a','b'),"c") (unsafeRun (pchar 'a' .&&. pchar 'b') "abc")
    , TestCase $ assertBool "Right .&&. Left" (isLeft $ parse (pchar 'a' .&&. pchar 'b') "aaa")
    , TestCase $ assertBool "Left .&&. Right" (isLeft $ parse (pchar 'a' .&&. pchar 'b') "bbb")
    ])

mapErrorTest = TestLabel "mapError" (TestList
    [ TestCase $ assertEqual "Right" ('a',"") (unsafeRun (pchar 'a' <?> (const "error")) "a")
    , TestCase $ assertEqual "Left" (Left "my-error") (parse (pchar 'a' <?> (const "my-error")) "b")
    ])

manyTest = TestLabel "many" (TestList 
    [ TestCase $ assertEqual "empty" ([],"") (unsafeRun (many $ pchar 'a') "")
    , TestCase $ assertEqual "mismatch" ([],"a") (unsafeRun (many $ pchar 'A') "a")
    , TestCase $ assertEqual "one" ("a","bc") (unsafeRun (many $ pchar 'a') "abc")
    , TestCase $ assertEqual "many" ("aaa","bc") (unsafeRun (many $ pchar 'a') "aaabc")
    ])

many1Test = TestLabel "many1" (TestList 
    [ TestCase $ assertBool "empty" (isLeft $ parse (many1 pdigit) "")
    , TestCase $ assertBool "mismatch" (isLeft $ parse (many1 pdigit) "a")
    , TestCase $ assertEqual "one" ("1","bc") (unsafeRun (many pdigit) "1bc")
    , TestCase $ assertEqual "many" ("123","abc") (unsafeRun (many pdigit) "123abc")
    ])

unsafeRun :: Parser a -> Input -> (a, Input)
unsafeRun p s = case run p s of
    Right (a, s) -> (a, s)
    Left err     -> error err

unsafeParse :: Parser a -> Input -> a
unsafeParse p = fst . unsafeRun p
