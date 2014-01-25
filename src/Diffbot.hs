{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module Diffbot
    (
    -- * Examples
    -- $examples

    -- * Perform a request
      diffbot
    -- * API
    -- ** Article
    , Article(..)
    , defArticle
    -- ** Front Page
    , FrontPage(..)
    , defFrontPage
    -- ** Image
    , Image(..)
    , defImage
    -- ** Product
    , Product(..)
    , defProduct
    -- ** Page Classifier
    , Classifier(..)
    , defClassifier
    -- * Type classes
    , Fields(..)
    , Post(..)
    , Timeout(..)
    -- * Datatypes
    , Content(..)
    , ContentType(..)
    -- * Internal
    , Request(..)
    , Req(..)
    -- * Exceptions
    , HttpException(..)
    ) where

import Data.Aeson
import Network.HTTP.Conduit (HttpException(..))

import Diffbot.Types
import Diffbot.Internal
import Diffbot.Article
import Diffbot.FrontPage
import Diffbot.Image
import Diffbot.Product
import Diffbot.Classifier


-- | The 'Object' type contains JSON objects:
--
-- >>> let token = "11111111111111111111111111111111"
-- >>> let url = "http://blog.diffbot.com/diffbots-new-product-api-teaches-robots-to-shop-online/"
-- >>> Just resp <- diffbot token url defaultArticle
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
    bot request [ ("token", Just token)
                , ("url",   Just url)
                ]

-- $examples
--
-- Just download information about the primary article content on the
-- submitted page:
--
-- > import Diffbot
-- >
-- > main = do
-- >     let token = "11111111111111111111111111111111"
-- >         url   = "http://blog.diffbot.com/diffbots-new-product-api-teaches-robots-to-shop-online/"
-- >     resp <- diffbot token url defaultArticle
-- >     print resp
--
-- You can control which fields are returned:
--
-- > main = do
-- >     let token  = "11111111111111111111111111111111"
-- >         url    = "http://blog.diffbot.com/diffbots-new-product-api-teaches-robots-to-shop-online/"
-- >         fields = Just "meta,querystring,images(*)"
-- >     resp <- diffbot token url $ setFields fields defaultArticle
-- >     print resp
--
-- If your content is not publicly available (e.g., behind a
-- firewall), you can POST markup for analysis directly:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > import Diffbot
-- >
-- > main = do
-- >     let token   = "11111111111111111111111111111111"
-- >         url     = "http://www.diffbot.com/our-apis/article"
-- >         content = Content TextPlain "Now is the time for all good robots to come to the aid of their -- oh never mind, run!"
-- >     -- Please note that the 'url' is still required, and will be used
-- >     -- to resolve any relative links contained in the markup.
-- >     resp <- diffbot token url $ setContent (Just content) defaultArticle
-- >     print resp
