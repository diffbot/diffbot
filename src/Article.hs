module Article where

import           Data.Default

import Types


-- | Used to extract clean article text from news article, blog post
-- and similar text-heavy web pages.
data Article = Article
    { articleToken    :: String
    -- ^ Developer token.

    , articleUrl      :: String
    -- ^ URL to process.

    , articleContent   :: Maybe Content
    -- ^ HTTP request method, e.g. GET, POST.

    , articleFields   :: Maybe String
    -- ^ Used to control which fields are returned by the API.

    , articleTimeout  :: Maybe Int
    -- ^ Set a value in milliseconds to terminate the response.
    }


instance Fields Article where
    fields        = articleFields
    setFields f a = a { articleFields = f }


instance Post Article where
    content        = articleContent
    setContent c a = a { articleContent = c }


instance Timeout Article where
    timeout        = articleTimeout
    setTimeout t a = a { articleTimeout = t }


instance Default Article where
    def = Article { articleToken    = ""
                  , articleUrl      = ""
                  , articleContent  = Nothing
                  , articleFields   = Nothing
                  , articleTimeout  = Nothing
                  }


instance Request Article where
    toReq r = Req { reqApi     = "http://api.diffbot.com/v2/article"
                  , reqToken   = articleToken r
                  , reqUrl     = articleUrl r
                  , reqContent = content r
                  , reqQuery   = fieldsQuery r ++ timeoutQuery r
                  }


mkArticle :: String -> String -> Article
mkArticle token url = def { articleToken = token
                          , articleUrl   = url
                          }
