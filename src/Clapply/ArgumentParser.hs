module Clapply.ArgumentParser
    ( module Clapply.Parser
    , module Clapply.ArgumentParser
    ) where

import Data.Bool (bool)
import Data.Bifunctor (first)

import Clapply.Prelude
import Clapply.Parser
import Clapply.StringParser

type Args = [String]
type ArgumentParser = Parser Args

liftS :: StringParser a -> ArgumentParser a
liftS p = liftP state 
  where
    state []     = Left "no input"
    state (x:xs) = case parse p x of
        Left err -> Left err
        Right a  -> Right (a,xs) 

popt :: ArgumentParser (Maybe String)
popt = liftP state
  where
    state []     = pure (Nothing, [])
    state (x:xs) = pure (Just x, xs)

text :: ArgumentParser String
text = liftP state
  where
    state []     = Left "no input"
    state (x:xs) = pure (x,xs)

switch :: [String] -> ArgumentParser (Maybe ())
switch cmds = f <$> popt
  where
    f opt = opt >>= bool Nothing (Just ()) . flip elem cmds
