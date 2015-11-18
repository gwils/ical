{-# LANGUAGE OverloadedStrings #-}

-- | All iCalendar types.

module ICal.Types where

import Data.Text (Text)
import Data.Aeson (ToJSON(..),object,(.=))

-- | Tree for representing iCal file.
data Object
  = Property !Text -- name
             !Text -- value
  | Object !Text -- name
           ![Object] -- values
  deriving (Show)

instance ToJSON Object where
  toJSON (Object name values) = object ["name" .= name,"values" .= values]
  toJSON (Property key value) = object ["key" .= key,"value" .= value]

-- | An iCalendar line.
data Line
  = Begin !Text -- object name
  | End !Text -- object name
  | Pair !Text -- name
         !Text -- value
  deriving (Show)
