{-# LANGUAGE OverloadedStrings #-}

module FrontPage where

import           Data.String

import           Data.Default
import qualified Data.ByteString.Char8 as BC
import           Network.HTTP.Conduit hiding (Request, method)
import           Network.HTTP.Types.URI

import           Types

-- | Takes in a multifaceted \"homepage\" and returns individual page
-- elements.
data FrontPage = FrontPage
    { frontPageToken  :: String
    , frontPageUrl    :: String
    , frontPageAll    :: Bool
    -- ^ Returns all content from page, including navigation and
    -- similar links that the Diffbot visual processing engine
    -- considers less important/non-core.
    , frontPageMethod :: Method
    }


instance Default FrontPage where
    def = FrontPage { frontPageToken  = ""
                    , frontPageUrl    = ""
                    , frontPageAll    = False
                    , frontPageMethod = Get
                    }


instance Request FrontPage where
    api _         = "http://www.diffbot.com/api/frontpage"
    token         = frontPageToken
    url           = frontPageUrl
    mkRequest t u = def { frontPageToken = t
                        , frontPageUrl   = u
                        }
    mkHttpRequest f = do
        req <- parseUrl u
        return $ addContent (method f) req { responseTimeout = Nothing }
      where
        u     = api f ++ BC.unpack query
        query = renderQuery True $ [ ("token",  Just . fromString $ token f)
                                   , ("url",    Just . fromString $ url f)
                                   , ("format", Just "json")
                                   ] ++ mkAllQuery (frontPageAll f)
        mkAllQuery b = if b then [ ("all", Nothing) ] else []


instance Post FrontPage where
    method = frontPageMethod
    setMethod m a = a { frontPageMethod = m }


mkFrontPage :: String -> String -> FrontPage
mkFrontPage = mkRequest
