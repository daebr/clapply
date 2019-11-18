module Clapply.PreludeTest where

import qualified Data.Functor as F

import Clapply.Prelude
import Test.HUnit

suite = TestLabel "Prelude" nestedMapTest

none :: Maybe Int
none = Nothing

nestedMapTest = TestLabel "<<$>>" (TestList
    [ TestCase $ assertEqual "Just . Just" (Just $ Just 2) ((+1) <<$>> Just (Just 1))
    , TestCase $ assertEqual "Just . None" (Just none) ((+1) <<$>> Just Nothing)
    ])
