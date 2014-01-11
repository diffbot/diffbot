{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Types where

import           Control.Applicative
import qualified Data.ByteString.Lazy as BL


-- | All information on how to connect to a Diffbot and what should be
-- sent in the request.
class Request a where
    toReq :: a -> Req


data Req = Req
    { reqApi     :: String
    , reqToken   :: String
    , reqUrl     :: String
    , reqContent :: Maybe Content
    , reqQuery   :: [(String, Maybe String)]
    }


-- FIXME: think about queries with key but without value
mkQuery :: String -> Maybe String -> [(String, Maybe String)]
mkQuery _ Nothing = []
mkQuery k v       = [(k, v)]


-- | Used to control which fields are returned by the API.
class Fields a where
    fields :: a -> Maybe String
    setFields :: Maybe String -> a -> a
    fieldsQuery :: a -> [(String, Maybe String)]
    fieldsQuery a = mkQuery "fields" $ fields a


class Post a where
    content :: a -> Maybe Content
    setContent :: Maybe Content -> a -> a


class Timeout a where
    timeout :: a -> Maybe Int
    setTimeout :: Maybe Int -> a -> a
    timeoutQuery :: a -> [(String, Maybe String)]
    timeoutQuery a = mkQuery "timeout" $ show <$> timeout a


data Content = Content
    { contentType :: ContentType
    -- ^ Type of content.
    , contentData :: BL.ByteString
    -- ^ Content to analyze.
    }


data ContentType = TextPlain
                 | TextHtml


-- strange behavior: the server returns error with corret "text/plain"
-- content-type
instance Show ContentType where
    show TextPlain = "text-plain"
    show TextHtml  = "text-html"
