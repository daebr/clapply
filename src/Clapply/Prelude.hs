module Clapply.Prelude where

import qualified Data.Functor as F

infixl 4 <<$>>
(<<$>>) :: (F.Functor f, F.Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap

const2 :: a -> b -> c -> a
const2 a _ _ = a
