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
-- >         req   = mkArticle token url
-- >     resp <- diffbot req
-- >     print resp
--
-- > -- You can control which fields are returned:
-- > import Diffbot
-- >
-- > main = do
-- >     let token = "11111111111111111111111111111111"
-- >         url   = "http://blog.diffbot.com/diffbots-new-product-api-teaches-robots-to-shop-online/"
-- >         req   = mkArticle token url
-- >         f     = Just "meta,querystring,images(*)"
-- >     resp <- diffbot $ setFields f req
-- >     print resp
--
-- > -- If your content is not publicly available (e.g., behind a
-- > -- firewall), you can POST markup for analysis directly:
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > import Diffbot
-- >
-- > main = do
-- >     let token = "11111111111111111111111111111111"
-- >         url   = "http://www.diffbot.com/our-apis/article"
-- >         -- Please note that the 'url' parameter is still required, and
-- >         -- will be used to resolve any relative links contained in the
-- >         -- markup.
-- >         req   = mkArticle token url
-- >         c     = Content TextPlain "Now is the time for all good robots to come to the aid of their -- oh never mind, run!"
-- >     resp <- diffbot $ setContent (Just c) req
-- >     print resp


module Diffbot
    ( -- * Perform a request
      diffbot
    , diffbotP
    -- * API
    -- ** Article
    , Article
    , mkArticle
    -- ** Front Page
    , FrontPage
    , mkFrontPage
    , frontPageAll
    -- ** Image
    , Image
    , mkImage
    -- ** Product
    , Product
    , mkProduct
    -- ** Page Classifier
    , Classifier
    , mkClassifier
    , classifierMode
    , classifierStats
    -- * Crawlbot
    , crawlbot
    , crawlbotP
    , Crawlbot(..)
    , mkCrawlbot
    , Limit(..)
    -- * Type classes
    , Fields(..)
    , Post(..)
    , Timeout(..)
    -- * Datatypes
    , Content(..)
    , ContentType(..)
    , Proxy(..)
    -- * Internal
    , Request(..)
    , Req(..)
    -- * Exceptions
    , HttpException(..)
    ) where

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

import           Types
import           Article
import           FrontPage
import           Image
import           Product
import           Classifier
import           Crawlbot


-- | The 'Object' type contains JSON objects:
--
-- >>> let token = "11111111111111111111111111111111"
-- >>> let url = "http://blog.diffbot.com/diffbots-new-product-api-teaches-robots-to-shop-online/"
-- >>> let req = mkArticle token url
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
diffbot :: Request a
        => String     -- ^ Developer token.
        -> String     -- ^ URL to process.
        -> a          -- ^ API
        -> IO (Maybe Object)
diffbot token url request =
    bot Nothing request [ ("token", Just token)
                        , ("url",   Just url)
                        ]


diffbotP :: Request a
         => String     -- ^ Developer token.
         -> String     -- ^ URL to process.
         -> Maybe Proxy
         -> a          -- ^ API
         -> IO (Maybe Object)
diffbotP token url p request =
    bot p request [ ("token", Just token)
                  , ("url",   Just url)
                  ]


crawlbot :: String -> Crawlbot -> IO (Maybe Response)
crawlbot token crawl =
    bot Nothing crawl [("token", Just token)]


crawlbotP :: String -> Maybe Proxy -> Crawlbot -> IO (Maybe Response)
crawlbotP token p crawl =
    bot p crawl [("token", Just token)]


bot :: (Request a, FromJSON b) =>
     Maybe Proxy -> a -> [(String, Maybe String)] -> IO (Maybe b)
bot p request query =
    withSocketsDo . either throwIO bot' $ mkHttpRequest p req
  where
    req = appendQuery query $ toReq request


bot' ::  FromJSON b => Http.Request -> IO (Maybe b)
bot' req =
    withManager $ \manager -> do
        response <- http req manager
        decode <$> (responseBody response $$+- CB.sinkLbs)


mkHttpRequest :: Maybe Proxy -> Req -> Either HttpException Http.Request
mkHttpRequest p (Req {..}) = do
    httpRequest <- parseUrl u
    return $ addContent reqContent httpRequest { responseTimeout = Nothing
                                               , proxy           = p
                                               }
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
