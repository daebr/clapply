module Clapply.ArgumentParser
    ( module Clapply.Parser
    , module Clapply.ArgumentParser
    ) where

import Data.Bool (bool)
import Data.Functor (void)
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

satisfy :: (String -> Bool) -> ArgumentParser String
satisfy f = liftP state
  where
    state [] = Left "no input"
    state (x:xs) = bool (Left "satisfy failed") (pure (x,xs)) (f x)

optional :: ArgumentParser a -> ArgumentParser (Maybe a)
optional p = liftP state
  where
    state [] = pure (Nothing, [])
    state xs = run (Just <$> p) xs

elementOf :: [String] -> ArgumentParser String
elementOf = satisfy . flip elem

text :: ArgumentParser String
text = liftP state
  where
    state []     = Left "no input"
    state (x:xs) = pure (x,xs)

switch :: [String] -> ArgumentParser (Maybe ())
switch = optional . void . elementOf

option :: [String] -> ArgumentParser String
option cmds = elementOf cmds &&. text
