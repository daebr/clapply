module Clapply.TestUtil where

import Clapply.Parser

unsafeRight :: Either e a -> a
unsafeRight (Right a) = a
unsafeRight _         = error "unsafeRight failed"

unsafeRun :: Parser s a -> s -> (a, s)
unsafeRun p = unsafeRight . run p

unsafeParse :: Parser s a -> s -> a
unsafeParse p = unsafeRight . parse p
