module Diffbot.FrontPage where

import Data.Maybe

import Diffbot.Types

-- | Takes in a multifaceted \"homepage\" and returns individual page
-- elements.
data FrontPage = FrontPage
    { frontPageAll    :: Bool
    -- ^ Returns all content from page, including navigation and
    -- similar links that the Diffbot visual processing engine
    -- considers less important/non-core.
    , frontPageContent :: Maybe Content
    , frontPageTimeout :: Maybe Int
    -- ^ Specify a value in milliseconds to override the default API
    -- timeout of 5000ms.
    }


instance Post FrontPage where
    content        = frontPageContent
    setContent c f = f { frontPageContent = c }


instance Timeout FrontPage where
    timeout        = frontPageTimeout
    setTimeout t f = f { frontPageTimeout = t }


instance Request FrontPage where
    toReq r = Req { reqApi     = "http://www.diffbot.com/api/frontpage"
                  , reqContent = content r
                  , reqQuery   = mkFrontPageQuery r
                  }


mkFrontPageQuery :: FrontPage -> [(String, Maybe String)]
mkFrontPageQuery f = catMaybes [ mkQueryBool "all"    (frontPageAll f)
                               , mkQuery     "format" (Just "json")
                               , timeoutQuery f
                               ]


defFrontPage :: FrontPage
defFrontPage = FrontPage { frontPageAll     = False
                         , frontPageContent = Nothing
                         , frontPageTimeout = Nothing
                         }
