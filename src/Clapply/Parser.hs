{-# LANGUAGE LambdaCase, TypeSynonymInstances #-}

module Clapply.Parser 
    ( Parser
    , ParseResult
    , Input
    , Error
    , parse
    , run
    , eval
    , exec
    , pend
    , pchar
    , pdigit
    , pstring
    , anyOf
    , many
    , many1
    , (<||>)
    , (.&&.)
    , (<?>)
    ) where

import qualified Data.List.NonEmpty as NE
import Control.Applicative ((<|>)) 
import Control.Monad.State.Lazy (StateT(..), runStateT)

type Input = String
type Error = String
type ParseResult = Either Error

type Parser = StateT Input ParseResult

run :: Parser a -> Input -> ParseResult (a, Input) 
run = runStateT

eval :: Parser a -> Input -> ParseResult a
eval p = fmap fst . run p

exec :: Parser a -> Input -> ParseResult Input
exec p = fmap snd . run p

parse :: Parser a -> Input -> ParseResult a
parse = eval

liftP :: (Input -> ParseResult (a, Input)) -> Parser a
liftP = StateT

failP :: Error -> Parser a
failP err = liftP (const $ Left err)

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = liftP state
  where
    state = \case
        []                 -> Left "no input"
        (x:xs) | p x       -> Right (x, xs)
               | otherwise -> Left "satisfy failed"

infixl 3 <||>
(<||>) :: Parser a -> Parser a -> Parser a
(<||>) = (<|>)

anyOf :: [Parser a] -> Parser a
anyOf [] = failP "anyOf empty"
anyOf xs = reduce (<|>) xs

andThen :: Parser a -> Parser b -> Parser (a,b)
andThen p1 p2 =
    p1 >>= (\a ->
    p2 >>= (\b ->
        pure (a,b)))

infix 3 .&&.
(.&&.) :: Parser a -> Parser b -> Parser (a,b)
(.&&.) = andThen

mapError :: (Error -> Error) -> Parser a -> Parser a
mapError f p = liftP state
  where
    state s = case run p s of
        Right (a,s') -> Right (a,s')
        Left e       -> Left (f e)

infixl 7 <?>
(<?>) :: Parser a -> (Error -> Error) -> Parser a
(<?>) = flip mapError

many :: Parser a -> Parser [a]
many p = ((:) <$> p <*> many p) <|> pure []

many1 :: Parser a -> Parser [a]
many1 p = (:) <$> p <*> many p

pend :: Parser ()
pend = liftP run
  where
    run = \case
        "" -> Right ((), "")
        s  -> Left "pend failed"

pchar :: Char -> Parser Char
pchar c = satisfy (== c)

pdigit :: Parser Char
pdigit = anyOf $ pchar <$> ['0'..'9']

pstring :: String -> Parser String
pstring = sequenceA . fmap pchar

reduce :: (Parser a -> Parser a -> Parser a) -> [Parser a] -> Parser a
reduce _ []     = fail "cannot reduce on empty list"
reduce _ [x]    = x
reduce f (x:xs) = f x (reduce f xs)
