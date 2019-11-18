module Clapply.TestUtil where

import Clapply.Parser

unsafeRight :: Show e => Either e a -> a
unsafeRight (Right a) = a
unsafeRight (Left e)  = error $ "unsafeRight failed: " <> show e

unsafeRun :: Parser s a -> s -> (a, s)
unsafeRun p = unsafeRight . run p

unsafeParse :: Parser s a -> s -> a
unsafeParse p = unsafeRight . parse p
