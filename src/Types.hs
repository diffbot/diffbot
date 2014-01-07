{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Data.Default
import qualified Data.ByteString.Lazy as BL


-- | All information on how to connect to a Diffbot and what should be
-- sent in the request.
data Request = Request
    { requestApi      :: Api

    , requestMethod   :: Method
    -- ^ HTTP request method, e.g. GET, POST.

    , requestToken    :: String
    -- ^ Developer token.

    , requestUrl      :: String
    -- ^ URL to process.

    , requestFields   :: Maybe String
    -- ^ Used to control which fields are returned by the API.

    , requestTimeout  :: Maybe Int
    -- ^ Set a value in milliseconds to terminate the response.

    , requestCallback :: Maybe String
    -- ^ Use for jsonp requests. Needed for cross-domain ajax.
    } deriving Show


instance Default Request where
  def = Request { requestApi      = Article
                , requestMethod   = Get
                , requestToken    = ""
                , requestUrl      = ""
                , requestFields   = Nothing
                , requestTimeout  = Nothing
                , requestCallback = Nothing
                }


data Api = Article
         | Frontpage
         | Image
           deriving Eq


instance Show Api where
    show Article   = "http://api.diffbot.com/v2/article"
    show Frontpage = "http://www.diffbot.com/api/frontpage"
    show Image     = "http://api.diffbot.com/v2/image"


-- | HTTP reequest method.
data Method = Get
            | Post { contentType :: ContentType
                   -- ^ Type of markup contained in the body.
                   , body :: BL.ByteString
                   -- ^ Markup to analyze.
                   }
              deriving Show


data ContentType = TextPlain
                 | TextHtml


-- strange behavior: the server returns error with corret "text/plain"
-- content-type
instance Show ContentType where
    show TextPlain = "text-plain"
    show TextHtml  = "text-html"
