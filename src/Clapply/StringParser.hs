{-# LANGUAGE LambdaCase #-}

module Clapply.StringParser
    ( StringParser
    , pend
    , pchar
    , pdigit
    , pstring
    )  where

import Clapply.Parser

type StringParser = Parser String

satisfy :: (Char -> Bool) -> StringParser Char
satisfy p = liftP state
  where
    state = \case
        []                 -> Left "no input"
        (x:xs) | p x       -> Right (x, xs)
               | otherwise -> Left "satisfy failed"

pend :: StringParser ()
pend = liftP run
  where
    run = \case
        "" -> Right ((), "")
        s  -> Left "pend failed"

pchar :: Char -> StringParser Char
pchar c = satisfy (== c)

pdigit :: StringParser Char
pdigit = anyOf $ pchar <$> ['0'..'9']

pstring :: String -> StringParser String
pstring = sequenceA . fmap pchar

