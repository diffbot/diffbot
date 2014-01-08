{-# LANGUAGE OverloadedStrings #-}

module Image where

import           Data.String

import           Data.Default
import qualified Data.ByteString.Char8 as BC
import           Network.HTTP.Conduit hiding (Request, method)
import           Network.HTTP.Types.URI

import           Types


-- | Analyzes a web page and returns its primary image(s).
data Image = Image
    { imageToken   :: String
    , imageUrl     :: String
    , imageFields  :: Maybe String
    , imageTimeout :: Maybe Int
    }


instance Default Image where
    def = Image { imageToken   = ""
                , imageUrl     = ""
                , imageFields  = Nothing
                , imageTimeout = Nothing
                }


instance Request Image where
    api _         = "http://api.diffbot.com/v2/image"
    token         = imageToken
    url           = imageUrl
    mkRequest t u = def { imageToken = t
                        , imageUrl   = u
                        }
    mkHttpRequest i = do
        req <- parseUrl u
        return req { responseTimeout = Nothing }
      where
        u     = api i ++ BC.unpack query
        query = renderSimpleQuery True $ [ ("token", fromString $ token i)
                                         , ("url",   fromString $ url i)
                                       ] ++ mkFieldsQuery (fields i)
                                         ++ mkTimeoutQuery (timeout i)


instance Fields Image where
    fields = imageFields
    setFields f i = i { imageFields = f }

instance Timeout Image where
    timeout = imageTimeout
    setTimeout t i = i { imageTimeout = t }

mkImage :: String -> String -> Image
mkImage = mkRequest
