{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

-- | The basic tokenizer.

module ICal.Tokenizer
  (-- * Top-level functions
   tokenizeObjectFromFile
  ,tokenizeObjectFromText
  ,tokenizeAesonFromText
  -- * Raw tokenizers
  ,objectTokenizer
  ,linesTokenizer
  ,lineTokenizer)
  where

import           Control.Monad.Fix
import           Data.Aeson (FromJSON(..),toJSON,fromJSON,Result(..))
import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as P
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           ICal.Types

-- | Tokenizer a complete document from a .ics file.
tokenizeObjectFromFile :: FilePath -> IO Object
tokenizeObjectFromFile fp =
  fmap tokenizeObjectFromText (T.readFile fp) >>=
  \case
    Left err -> error err
    Right ok -> return ok

-- | Tokenizer a complete document.
tokenizeObjectFromText :: Text -> Either String Object
tokenizeObjectFromText t =
  do ls <- P.parseOnly linesTokenizer t
     (object,remainder) <- objectTokenizer ls
     if null remainder
        then return object
        else Left ("Unexpected extraneous input: " ++ show remainder)

-- | Tokenize an Aeson instance from the document.
tokenizeAesonFromText :: FromJSON a => Text -> Either String a
tokenizeAesonFromText t =
  do doc <- tokenizeObjectFromText t
     case fromJSON (toJSON doc) of
       Error e -> Left e
       Success a -> Right a

-- | Tokenize a list of lines into an object.
objectTokenizer :: [Line] -> Either String (Object,[Line])
objectTokenizer [] = Left "Unexpected end of input."
objectTokenizer (Begin name:linesInAndAfterObject) =
  do (values,linesAfterObject) <-
       fix (\loop ->
              \case
                [] -> return ([],[])
                nextLineSet@(next:linesAfterObject) ->
                  case next of
                    End{} -> return ([],linesAfterObject)
                    _ ->
                      do (x,linesAfterChildObject) <- objectTokenizer nextLineSet
                         (xs,linesAfterRestOfChildren) <-
                           loop linesAfterChildObject
                         return (x : xs,linesAfterRestOfChildren))
           linesInAndAfterObject
     return (Object name values,linesAfterObject)
objectTokenizer (Pair key value:linesAfterPair) =
  return (Property key value,linesAfterPair)
objectTokenizer (End name:_) =
  Left ("Unexpected end of object: " ++ show name)

-- | Tokenize lines of iCalendar format.
linesTokenizer :: Parser [Line]
linesTokenizer = P.many1 lineTokenizer

-- | Tokenize a single line.
lineTokenizer :: Parser Line
lineTokenizer =
  do (key,value) <- propertyTokenizer
     case key of
       "BEGIN" -> return (Begin value)
       "END" -> return (End value)
       _ -> return (Pair key value)

-- | Tokenize a (possibly-mult-line) property.
propertyTokenizer :: Parser (Text,Text)
propertyTokenizer =
  do key <- P.takeWhile1 (not . propertySeparator)
     _ <- P.satisfy propertySeparator
     fmap ((key,) . T.concat)
          (fix (\loop ->
                  do value <- P.takeTill newline
                     _ <- P.takeWhile1 newline
                     mnext <- P.peekChar
                     case mnext of
                       Just ' ' ->
                         do _ <- P.anyChar
                            rest <- loop
                            return (value : rest)
                       _ -> return [value]))
  where propertySeparator c = c == ':' || c == ';'
        newline c = c == '\r' || c == '\n'
