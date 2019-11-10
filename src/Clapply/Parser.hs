{-# LANGUAGE LambdaCase #-}

module Clapply.Parser 
    ( parse
    , eval
    , exec
    , pend
    , pchar
    , many
    ) where

import qualified Data.List.NonEmpty as NE
import Control.Monad.State.Lazy (State, state, runState, evalState, execState)

type ParseResult a = Either String a

type Parser a = State String (ParseResult a)

parse :: Parser a -> String -> (Either String a, String)
parse = runState

eval :: Parser a -> String -> Either String a
eval = evalState

exec :: Parser a -> String -> String
exec = execState

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = state run
  where
    run = \case
        []                 -> (Left "satisfy input empty", [])
        (x:xs) | p x       -> (Right x, xs)
               | otherwise -> (Left "satisfy failed", xs)

many :: Parser a -> Parser [a]
many = error "???"

pend :: Parser ()
pend = state run
  where
    run = \case
        "" -> (Right (), "")
        s  -> (Left "pend failed", s) 

pchar :: Char -> Parser Char
pchar c = satisfy (== c)
