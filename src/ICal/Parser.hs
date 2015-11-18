{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

-- | The basic parser.

module ICal.Parser
  (-- * Types
   Parser
  ,ParseError(..)
  -- * Handy starter functions
  ,parseEither
  -- * Combinators
  ,begin
  ,object
  ,objects
  ,property
  ,properties
  -- * Parser library
  ,local
  ,parseError
  ,getState
  ,putState)
  where

import Control.Applicative
import Control.Monad.State.Strict
import Control.Monad.Trans.Either
import Data.List
import Data.Maybe
import Data.Text (Text)
import ICal.Types

-- | A parse error.
data ParseError
  = ExpectedObject !Text
  | ExpectedProperty !Text
  | GeneralProblem !Text
  deriving (Show)

-- | Parse some iCalendar thing.
parseEither
  :: Monad m
  => s -> Parser m s a -> m (Either ParseError a)
parseEither s p =
  evalStateT (runEitherT (runParser p))
             s

-- | Parser type.
newtype Parser m s a =
  Parser {runParser :: EitherT ParseError (StateT s m) a}
  deriving (Monad,Applicative,Functor)

-- | Left branch failing resets the state.
instance Monad m => Alternative (Parser m s) where
  Parser x <|> Parser y =
    Parser (do s <- get
               r <-
                 EitherT (do r <- runEitherT x
                             return (Right r))
               case r of
                 Left{} -> do put s
                              y
                 Right ok -> return ok)
  empty = parseError (GeneralProblem "empty parser")

-- | Lookup a property.
property :: Monad m
         => Text -- ^ Key
         -> Parser m [Object] Text -- ^ The value of that property.
property !key =
  do os <- getState
     case listToMaybe
            (mapMaybe (\case
                         Property name value
                           | name == key -> Just value
                         _ -> Nothing)
                      os) of
       Nothing -> parseError (ExpectedProperty key)
       Just x -> return x

-- | Get all values of a property.
properties :: Monad m
           => Text -- ^ Key
           -> Parser m [Object] [Text] -- ^ The values of that property.
properties !key =
  do os <- getState
     return (mapMaybe (\case
                         Property name value
                           | name == key -> Just value
                         _ -> Nothing)
                      os)

-- | Lookup an object with this name in the current object's children,
-- then run with that object as the context.
object :: Monad m => Text -> Parser m [Object] a -> Parser m [Object] a
object !name m =
  do os <- getState
     case find (\case
                   Object name' _ -> name' == name
                   _ -> False) os of
       Just (Object _ children) ->
         local children m
       _ -> parseError (ExpectedObject name)

-- | Lookup objects with this name in the current object's children,
-- then run with that object as the context.
objects :: Monad m => Text -> Parser m [Object] a -> Parser m [Object] [a]
objects !name m =
  do os <- getState
     case mapMaybe (\case
                      Object name' children
                        | name' == name -> Just children
                      _ -> Nothing)
                   os of
       cs -> mapM (\children -> local children m) cs

-- | Require the given object name to exist and run in that context.
begin :: Monad m => Text -> Parser m [Object] a -> Parser m Object a
begin !name m =
  do o <- getState
     case o of
       Object name' children ->
         if name' == name
            then local children m
            else parseError (ExpectedObject name)
       _ -> parseError (ExpectedObject name)

-- | Use a local state of a different type.
local :: Monad m
      => t -> Parser m t a -> Parser m s a
local temp m =
  Parser (EitherT (StateT (\orig ->
                             do (result,_new) <-
                                  runStateT (runEitherT (runParser m)) temp
                                return (result,orig))))

-- | Throw a parse error.
parseError :: Monad m => ParseError -> Parser m o a
parseError = Parser . left

-- | Get the current state.
getState :: Monad m => Parser m s s
getState = Parser get

-- | Put a new state.
putState :: Monad m => s -> Parser m s ()
putState = Parser . put
