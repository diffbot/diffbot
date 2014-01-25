{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Diffbot.Internal where

import           Control.Applicative
import           Control.Exception (throwIO)
import qualified Data.ByteString.Char8 as BC
import           Data.String

import           Data.Aeson
import           Data.Conduit
import qualified Data.Conduit.Binary as CB
import           Network (withSocketsDo)
import qualified Network.HTTP.Conduit as Http (Request, method)
import           Network.HTTP.Conduit hiding (Request, Response, method)
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.QueryLike
import           Network.HTTP.Types.URI

import           Diffbot.Types


bot :: (Request a, FromJSON b) =>
    a -> [(String, Maybe String)] -> IO (Maybe b)
bot request query =
    withSocketsDo . either throwIO bot' $ mkHttpRequest req
  where
    req = appendQuery query $ toReq request


bot' ::  FromJSON b => Http.Request -> IO (Maybe b)
bot' req =
    withManager $ \manager -> do
        response <- http req manager
        decode <$> (responseBody response $$+- CB.sinkLbs)


mkHttpRequest :: Req -> Either HttpException Http.Request
mkHttpRequest (Req {..}) = do
    httpRequest <- parseUrl u
    return $ addContent reqContent httpRequest { responseTimeout = Nothing }
  where
    u     = reqApi ++ BC.unpack query
    query = renderQuery True $ toQuery reqQuery


addContent :: Maybe Content -> Http.Request -> Http.Request
addContent m req =
    case m of
      Nothing ->
          req { Http.method =    "GET" }
      Just (Content {..}) ->
          req { Http.method    = "POST"
              , requestHeaders = [(hContentType, fromString $ show contentType)]
              , requestBody    = RequestBodyLBS contentData
              }
