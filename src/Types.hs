{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Data.Default
import           Network.HTTP.Types.Method (Method)

-- | All information on how to connect to a Diffbot and what should be
-- sent in the request.
data Request = Request
    { requestApi      :: Api

--    , requestMethod   :: Method
    -- ^ HTTP request method, eg GET, POST.

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

data Req = Req {
      reqApi :: String
    } deriving Show

instance Default Request where
  def = Request { requestApi      = Article
--                , requestMethod   = "GET"
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
