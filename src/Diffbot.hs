{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Some examples:
--
-- > -- Just download information about the primary article content on the
-- > -- submitted page.
-- > import Diffbot
-- >
-- > main = do
-- >     let token = "11111111111111111111111111111111"
-- >         url   = "http://blog.diffbot.com/diffbots-new-product-api-teaches-robots-to-shop-online/"
-- >         req   = mkRequest token url
-- >     resp <- diffbot req
-- >     print resp
--
-- > -- You can control which fields are returned:
-- > main = do
-- >     let token = "11111111111111111111111111111111"
-- >         url   = "http://blog.diffbot.com/diffbots-new-product-api-teaches-robots-to-shop-online/"
-- >         req   = mkRequest token url
-- >     resp <- diffbot req { requestFields = Just "meta,querystring,images(*)" }
-- >     print resp

module Diffbot
    ( -- * Perform a request
      diffbot
    -- * Datatypes
    , Request(..)
    , Api(..)
    -- * Utility functions
    , mkRequest
    -- * Exceptions
    , HttpException(..)
    ) where

import           Control.Applicative
import           Control.Exception (throwIO)

import           Data.Aeson
import           Data.Conduit
import qualified Data.Conduit.Binary as CB
import           Data.Default
import qualified Network.HTTP.Conduit as Http (Request)
import           Network.HTTP.Conduit hiding (Request)
import           Network (withSocketsDo)
import qualified Data.ByteString.Char8 as BC
import           Network.HTTP.Types.URI

import           Types


mkRequest :: String -> String -> Request
mkRequest token url = def { requestToken = token
                          , requestUrl   = url
                          }


-- FIXME: requestMethod, requestCallback
mkHttpRequest :: Request -> Either HttpException Http.Request
mkHttpRequest (Request {..}) = do
    req <- parseUrl url
    return req {responseTimeout = Nothing}
  where
      url   = show requestApi ++ query
      query = BC.unpack . renderQuery True $ [ ("token", Just $ BC.pack requestToken)
                                             , ("url",   Just $ BC.pack requestUrl)
                                             ] ++ mkFieldsQuery requestFields
                                               ++ mkTimeoutQuery requestTimeout


mkFieldsQuery :: Maybe String -> [QueryItem]
mkFieldsQuery (Just s) = [("fields", Just $ BC.pack s)]
mkFieldsQuery Nothing  = []


mkTimeoutQuery :: Maybe Int -> [QueryItem]
mkTimeoutQuery (Just t) = [("timeout", Just . BC.pack $ show t)]
mkTimeoutQuery Nothing  = []

-- | The 'Object' type contains JSON objects:
--
-- >>> let token = "11111111111111111111111111111111"
-- >>> let url = "http://blog.diffbot.com/diffbots-new-product-api-teaches-robots-to-shop-online/"
-- >>> let req = mkRequest token url
-- >>> Just resp <- diffbot req
-- >>> resp
-- fromList [("author",String "John Davi"),("title",String "Diffbot\8217s New Product API Teaches Robots to Shop Online"),...
--
-- You can extract values from it with a parser using 'parse',
-- 'parseEither' or, in this example, 'parseMaybe' from aeson package:
--
-- > getInfo :: Object -> Maybe String
-- > getInfo resp = flip parseMaybe resp $ \obj -> do
-- >     author <- obj .: "author"
-- >     title  <- obj .: "title"
-- >     return $ title ++ ", by " ++ author
--
-- >>> getInfo resp
-- Just "Diffbot\8217s New Product API Teaches Robots to Shop Online, by John Davi"
diffbot :: Request -> IO (Maybe Object)
diffbot req =
    withSocketsDo . either throwIO diffbot' $ mkHttpRequest req


diffbot' :: Http.Request -> IO (Maybe Object)
diffbot' req = withManager $ \manager -> do
    response <- http req manager
    decode <$> (responseBody response $$+- CB.sinkLbs)
