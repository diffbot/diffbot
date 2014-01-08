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
-- >         m     = Post TextPlain "Now is the time for all good robots to come to the aid of their -- oh never mind, run!"
-- >     resp <- diffbot $ setMethod m req
-- >     print resp

module Diffbot
    (  -- * Perform a request
      diffbot
    -- * API
    , Request
    , Article
    , mkArticle
    , FrontPage
    , mkFrontPage
    , Image
    , mkImage
    -- * Type classes
    , Fields(..)
    , Post(..)
    , Timeout(..)
    -- * Datatypes
    , Method(..)
    , ContentType(..)
    -- * Exceptions
    , HttpException(..)
    ) where

import           Control.Applicative
import           Control.Exception (throwIO)

import           Data.Aeson
import           Data.Conduit
import qualified Data.Conduit.Binary as CB
import           Network (withSocketsDo)
import qualified Network.HTTP.Conduit as Http (Request)
import           Network.HTTP.Conduit hiding (Request, method)

import           Types
import           Article
import           FrontPage
import           Image
import           Product

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
diffbot :: Request a => a -> IO (Maybe Object)
diffbot req =
    withSocketsDo . either throwIO diffbot' $ mkHttpRequest req


diffbot' :: Http.Request -> IO (Maybe Object)
diffbot' req = do
    withManager $ \manager -> do
        response <- http req manager
        decode <$> (responseBody response $$+- CB.sinkLbs)
