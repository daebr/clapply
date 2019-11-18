module Clapply.Prelude where

import qualified Data.Functor as F

infixl 4 <<$>>
(<<$>>) :: (F.Functor f, F.Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap
