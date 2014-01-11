module FrontPage where

import Data.Default

import Types

-- | Takes in a multifaceted \"homepage\" and returns individual page
-- elements.
data FrontPage = FrontPage
    { frontPageAll    :: Bool
    -- ^ Returns all content from page, including navigation and
    -- similar links that the Diffbot visual processing engine
    -- considers less important/non-core.
    , frontPageContent :: Maybe Content
    }


instance Post FrontPage where
    content        = frontPageContent
    setContent c f = f { frontPageContent = c }


instance Default FrontPage where
    def = FrontPage { frontPageAll     = False
                    , frontPageContent = Nothing
                    }


instance Request FrontPage where
    toReq r = Req { reqApi     = "http://www.diffbot.com/api/frontpage"
                  , reqContent = content r
                  , reqQuery   = mkFrontPageQuery r
                  }


mkFrontPageQuery :: FrontPage -> [(String, Maybe String)]
mkFrontPageQuery f = allQuery ++ formatQuery
  where
    allQuery    = if frontPageAll f then [("all", Nothing)] else []
    formatQuery = [ ("format", Just "json") ]


mkFrontPage :: FrontPage
mkFrontPage = def
