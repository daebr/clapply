module Clapply.ParserTest where

import Data.Bool (bool)
import Data.Either (isLeft)
import Control.Applicative ((<|>))

import Clapply.Parser
import Clapply.TestUtil
import Test.HUnit

suite = TestLabel "Paresr" (TestList
    [ runTest
    , evalTest
    , execTest
    , parseTest
    , andThenTest
    , orElseTest
    , anyOfTest
    , mapErrorTest
    , manyTest
    , many1Test
    ])

type Quantity = Int
type Warehouse = Parser Quantity

order :: Quantity -> Warehouse Quantity
order qty = liftP state
  where
    state wh = bool (Left "too large") (Right (qty, wh - qty)) (wh >= qty)

runTest = TestLabel "parse" (TestList
    [ TestCase $ assertEqual "success" (Right (1,4)) (run (order 1) 5)
    , TestCase $ assertBool "failure" (isLeft $ run (order 1) 0)
    ])

evalTest = TestLabel "eval" (TestList
    [ TestCase $ assertEqual "success" (Right 1) (eval (order 1) 5)
    , TestCase $ assertBool "failure" (isLeft $ eval (order 1) 0)
    ])

execTest = TestLabel "exec" (TestList
    [ TestCase $ assertEqual "success" (Right 4) (exec (order 1) 5)
    , TestCase $ assertBool "failure" (isLeft $ exec (order 1) 0)
    ])

parseTest = TestLabel "parse" (TestList
    [ TestCase $ assertEqual "success" (Right 1) (parse (order 1) 5)
    , TestCase $ assertBool "failure" (isLeft $ parse (order 1) 0)
    ])

orElseTest = TestLabel "orElse" (TestList
    [ TestCase $ assertEqual "Right <||> Right" (1,4) (unsafeRun (order 1 <||> order 2) 5)
    , TestCase $ assertEqual "Right <||> Left" (1, 0) (unsafeRun (order 1 <||> order 2) 1)
    , TestCase $ assertEqual "Left <||> Right" (1,0) (unsafeRun (order 2 <||> order 1) 1)
    , TestCase $ assertBool "Left <||> Left" (isLeft $ parse (order 4 <||> order 5) 1)
    , TestCase $ assertEqual "alternative" (1,0) (unsafeRun (order 2 <|> order 1) 1)
    ])

anyOfTest = TestLabel "anyOf" (TestList
    [ TestCase $ assertEqual "[Right,Right]" (1,4) (unsafeRun (anyOf [order 1, order 2]) 5)
    , TestCase $ assertBool "[Left, Left]" (isLeft $ parse (anyOf [order 4, order 5]) 1)
    , TestCase $ assertEqual "[Left, Right]" (1,0) (unsafeRun (anyOf [order 2, order 1]) 1)
    ])

andThenTest = TestLabel "andThen" (TestList
    [ TestCase $ assertEqual "Right .&&. Right" ((1,2),6) (unsafeRun (order 1 .&&. order 2) 9)
    , TestCase $ assertBool "Right .&&. Left" (isLeft $ parse (order 1 .&&. order 2) 2)
    , TestCase $ assertBool "Left .&&. Right" (isLeft $ parse (order 2 .&&. order 1) 1)
    , TestCase $ assertEqual ".&&" (1,6) (unsafeRun (order 1 .&& order 2) 9)
    , TestCase $ assertEqual "&&." (2,6) (unsafeRun (order 1 &&. order 2) 9)
    ])

mapErrorTest = TestLabel "mapError" (TestList
    [ TestCase $ assertEqual "Right" (1,0) (unsafeRun (order 1 <?> "my-error") 1)
    , TestCase $ assertEqual "Left" (Left "my-error") (parse (order 5 <?> "my-error") 1)
    ])

manyTest = TestLabel "many" (TestList 
    [ TestCase $ assertEqual "none" ([],1) (unsafeRun (many $ order 2) 1)
    , TestCase $ assertEqual "one" ([2],0) (unsafeRun (many $ order 2) 2)
    , TestCase $ assertEqual "many" ([2,2],1) (unsafeRun (many $ order 2) 5)
    ])

many1Test = TestLabel "many1" (TestList 
    [ TestCase $ assertBool "none" (isLeft $ parse (many1 $ order 2) 1)
    , TestCase $ assertEqual "one" ([2],0) (unsafeRun (many1 $ order 2) 2)
    , TestCase $ assertEqual "many" ([2,2],1) (unsafeRun (many1 $ order 2) 5)
    ])
