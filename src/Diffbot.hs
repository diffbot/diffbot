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
--
-- > -- If your content is not publicly available (e.g., behind a
-- > -- firewall), you can POST markup for analysis directly:
-- > main = do
-- >     let token = "11111111111111111111111111111111"
-- >         url   = "http://www.diffbot.com/our-apis/article"
-- >         -- Please note that the 'url' parameter is still required, and
-- >         -- will be used to resolve any relative links contained in the
-- >         -- markup.
-- >         req   = mkRequest token url
-- >     resp <- diffbot req { requestMethod = Post TextPlain "Now is the time for all good robots to come to the aid of their -- oh never mind, run!"
-- >     print resp

module Diffbot
    ( -- * Perform a request
      diffbot
    -- * Datatypes
    , Request(..)
    , Api(..)
    , Method(..)
    , ContentType(..)
    -- * Utility functions
    , mkRequest
    -- * Exceptions
    , HttpException(..)
    ) where

import           Control.Applicative
import           Control.Exception (throwIO)
import           Data.String

import           Data.Aeson
import           Data.Conduit
import qualified Data.Conduit.Binary as CB
import           Data.Default
import           Network (withSocketsDo)
import qualified Network.HTTP.Conduit as Http (Request)
import           Network.HTTP.Conduit hiding (Request)
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.URI
import qualified Data.ByteString.Char8 as BC

import           Types


mkRequest :: String -> String -> Request
mkRequest token url = def { requestToken = token
                          , requestUrl   = url
                          }


-- FIXME: requestMethod, requestCallback
mkHttpRequest :: Request -> Either HttpException Http.Request
mkHttpRequest (Request {..}) = do
    req <- parseUrl url
    case requestMethod of
      Get ->
          return req { method = "GET"
                     , responseTimeout = Nothing
                     }
      Post {..} ->
          return req { method = "POST"
                     , requestHeaders = [(hContentType, fromString $ show contentType)]
                     , requestBody = RequestBodyLBS body
                     , responseTimeout = Nothing
                     }
  where
      url   = show requestApi ++ BC.unpack query
      query = renderSimpleQuery True $ [ ("token", fromString requestToken)
                                       , ("url",   fromString requestUrl)
                                       ] ++ mkFieldsQuery requestFields
                                         ++ mkTimeoutQuery requestTimeout


mkFieldsQuery :: Maybe String -> SimpleQuery
mkFieldsQuery (Just s) = [("fields", fromString s)]
mkFieldsQuery Nothing  = []


mkTimeoutQuery :: Maybe Int -> SimpleQuery
mkTimeoutQuery (Just t) = [("timeout", fromString $ show t)]
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
diffbot' req = do
  print req
  withManager $ \manager -> do
    response <- http req manager
    decode <$> (responseBody response $$+- CB.sinkLbs)
