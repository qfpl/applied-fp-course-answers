{-# LANGUAGE OverloadedStrings #-}
module FirstApp.Types
  ( Topic
  , CommentText
  , ContentType (..)
  , RqType (..)
  , Error (..)
  , mkTopic
  , getTopic
  , mkCommentText
  , getCommentText
  , renderContentType
  ) where

import           Data.ByteString (ByteString)
import           Data.Text       (Text)

-- Working through the specification for our application, what are the
-- types of requests we're going to handle?

-- We have to be able to:
-- - Comment on a given topic
-- - View a topic and its comments
-- - List the current topics

-- To that end, we will create the following constructors for our RqType:

-- AddRq : Which needs the target topic, and the body of the comment.
-- ViewRq : Which needs the topic being requested.
-- ListRq : Which doesn't need anything and lists all of the current topics.

data RqType
  = AddRq Topic CommentText
  | ViewRq Topic
  | ListRq

-- Not everything goes according to plan, but it's important that our
-- types reflect when errors can be introduced into our program. Additionally
-- it's useful to be able to be descriptive about what went wrong.

-- Leave this type empty for now, and we'll fill in the error constructors we
-- need as we progress through the application.
data Error
  = UnknownRoute
  | EmptyCommentText
  | EmptyTopic
  deriving Show

-- Provide a type to list our response content types so we don't try to
-- do the wrong thing with what we meant to be used as text/JSON etc.
data ContentType
  = PlainText
  | JSON

-- The ``ContentType`` constructors don't match what is required for the header
-- information, so write a function that will take our ``ContentType`` and
-- produce the correct value for the header.
renderContentType
  :: ContentType
  -> ByteString
-- renderContentType = error "renderContentType not implemented"
renderContentType PlainText = "text/plain"
renderContentType JSON      = "text/json"

newtype Topic = Topic Text
  deriving Show

newtype CommentText = CommentText Text
  deriving Show

-- We can choose to *not* export the constructor for a data type and instead
-- provide a function of our own. In our case, we're not interested in empty
-- `Text` values so we will eliminate them with a special constructor and return
-- an error if an empty input is provided.
--
-- The export list at the top of this file demonstrates how to export a type,
-- but not export the constructor.

mkTopic
  :: Text
  -> Either Error Topic
mkTopic "" =
  Left EmptyTopic
mkTopic ti =
  Right (Topic ti)

getTopic
  :: Topic
  -> Text
getTopic (Topic t) =
  t

mkCommentText
  :: Text
  -> Either Error CommentText
mkCommentText "" =
  Left EmptyCommentText
mkCommentText ct =
  Right (CommentText ct)

getCommentText
  :: CommentText
  -> Text
getCommentText (CommentText t) =
  t
