{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Types where

import           Data.String

import qualified Data.ByteString.Lazy as BL
import qualified Network.HTTP.Conduit as Http (Request, method)
import           Network.HTTP.Conduit hiding (Request, method)
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.URI


-- | All information on how to connect to a Diffbot and what should be
-- sent in the request.
class Request a where
    api           :: a -> String
    token         :: a -> String
    url           :: a -> String
    mkRequest     :: String -> String -> a
    mkHttpRequest :: a -> Either HttpException Http.Request


-- | Used to control which fields are returned by the API.
class Fields a where
    fields :: a -> Maybe String
    setFields :: Maybe String -> a -> a


mkFieldsQuery :: Maybe String -> SimpleQuery
mkFieldsQuery (Just s) = [("fields", fromString s)]
mkFieldsQuery Nothing  = []


class Post a where
    method :: a -> Method
    setMethod :: Method -> a -> a


addContent :: Method -> Http.Request -> Http.Request
addContent m req =
    case m of
      Get ->
          req { Http.method = "GET" }  -- FIXME: maybe without this?
      Post {..} ->
          req { Http.method = "POST"
              , requestHeaders = [(hContentType, fromString $ show contentType)]
              , requestBody = RequestBodyLBS body
              , responseTimeout = Nothing
              }


class Timeout a where
    timeout :: a -> Maybe Int
    setTimeout :: Maybe Int -> a -> a


mkTimeoutQuery :: Maybe Int -> SimpleQuery
mkTimeoutQuery (Just t) = [("timeout", fromString $ show t)]
mkTimeoutQuery Nothing  = []


-- | HTTP reequest method.
data Method = Get
            | Post { contentType :: ContentType
                   -- ^ Type of markup contained in the body.
                   , body :: BL.ByteString
                   -- ^ Markup to analyze.
                   }
              deriving Show


-- data Content = Content
--     { contentType :: ContentType
--     -- ^ Type of content.
--     , contentData :: BL.ByteString
--     -- ^ Content to analyze.
--     }


data ContentType = TextPlain
                 | TextHtml


-- strange behavior: the server returns error with corret "text/plain"
-- content-type
instance Show ContentType where
    show TextPlain = "text-plain"
    show TextHtml  = "text-html"
