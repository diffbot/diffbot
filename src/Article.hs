{-# LANGUAGE OverloadedStrings #-}

module Article where

import           Data.String

import           Data.Default
import qualified Data.ByteString.Char8 as BC
import           Network.HTTP.Conduit hiding (Request, method)
import           Network.HTTP.Types.URI

import Types


-- | Used to extract clean article text from news article, blog post
-- and similar text-heavy web pages.
data Article = Article
    { articleToken    :: String
    -- ^ Developer token.

    , articleUrl      :: String
    -- ^ URL to process.

    , articleMethod   :: Method
    -- ^ HTTP request method, e.g. GET, POST.

    , articleFields   :: Maybe String
    -- ^ Used to control which fields are returned by the API.

    , articleTimeout  :: Maybe Int
    -- ^ Set a value in milliseconds to terminate the response.
    }


instance Default Article where
    def = Article { articleToken    = ""
                  , articleUrl      = ""
                  , articleMethod   = Get
                  , articleFields   = Nothing
                  , articleTimeout  = Nothing
                  }


instance Request Article where
    api _         = "http://api.diffbot.com/v2/article"
    token         = articleToken
    url           = articleUrl
    mkRequest t u = def { articleToken = t
                        , articleUrl   = u
                        }
    mkHttpRequest a = do
        req <- parseUrl u
        return $ addContent (method a) req { responseTimeout = Nothing }
      where
        u     = api a ++ BC.unpack query
        query = renderSimpleQuery True $ [ ("token", fromString $ token a)
                                         , ("url",   fromString $ url a)
                                       ] ++ mkFieldsQuery (fields a)
                                         ++ mkTimeoutQuery (timeout a)


instance Fields Article where
    fields = articleFields
    setFields f a = a { articleFields = f }


instance Post Article where
    method = articleMethod
    setMethod m a = a { articleMethod = m }

instance Timeout Article where
    timeout = articleTimeout
    setTimeout t a = a { articleTimeout = t }

mkArticle :: String -> String -> Article
mkArticle = mkRequest
