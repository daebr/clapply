{-# LANGUAGE LambdaCase, TypeSynonymInstances #-}

module Clapply.Parser 
    ( module Control.Applicative
    , Parser
    , ParseResult
    , Error
    , parse
    , run
    , eval
    , exec
    , liftP
    , failP
    , anyOf
    , many
    , many1
    , reduce
    , (<||>)
    , (.&&.)
    , (.&&)
    , (&&.)
    , (<?>)
    ) where

import Control.Applicative ((<|>)) 
import Control.Monad.State.Lazy (StateT(..), runStateT)

type Error = String
type ParseResult = Either Error
type Parser input = StateT input ParseResult

run :: Parser s a -> s -> ParseResult (a, s) 
run = runStateT

eval :: Parser s a -> s -> ParseResult a
eval p = fmap fst . run p

exec :: Parser s a -> s -> ParseResult s
exec p = fmap snd . run p

parse :: Parser s a -> s -> ParseResult a
parse = eval

liftP :: (s -> ParseResult (a, s)) -> Parser s a
liftP = StateT

failP :: Error -> Parser s a
failP err = liftP (const $ Left err)

infixl 3 <||>
(<||>) :: Parser s a -> Parser s a -> Parser s a
(<||>) = (<|>)

anyOf :: [Parser s a] -> Parser s a
anyOf [] = failP "anyOf empty"
anyOf xs = reduce (<|>) xs

andThen :: Parser s a -> Parser s b -> Parser s (a,b)
andThen p1 p2 =
    p1 >>= (\a ->
    p2 >>= (\b ->
        pure (a,b)))

infix 3 .&&.
(.&&.) :: Parser s a -> Parser s b -> Parser s (a,b)
(.&&.) = andThen

infixl 3 .&&
(.&&) :: Parser s a -> Parser s a -> Parser s a
p1 .&& p2 = p1 <* p2

infixl 3 &&.
(&&.) :: Parser s a -> Parser s a -> Parser s a
p1 &&. p2 = p1 *> p2

setError :: Error -> Parser s a -> Parser s a
setError e p = liftP state
  where
    state s = case run p s of
        Right (a,s') -> Right (a,s')
        Left _       -> Left e

infixl 7 <?>
(<?>) :: Parser s a -> Error -> Parser s a
(<?>) = flip setError 

many :: Parser s a -> Parser s [a]
many p = ((:) <$> p <*> many p) <|> pure []

many1 :: Parser s a -> Parser s [a]
many1 p = (:) <$> p <*> many p

reduce :: (Parser s a -> Parser s a -> Parser s a) -> [Parser s a] -> Parser s a
reduce _ []     = failP "cannot reduce on empty list"
reduce _ [x]    = x
reduce f (x:xs) = f x (reduce f xs)
