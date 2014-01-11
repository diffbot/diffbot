module Image where

import Data.Default

import Types


-- | Analyzes a web page and returns its primary image(s).
data Image = Image
    { imageToken   :: String
    , imageUrl     :: String
    , imageFields  :: Maybe String
    , imageTimeout :: Maybe Int
    }


instance Fields Image where
    fields        = imageFields
    setFields f i = i { imageFields = f }


instance Timeout Image where
    timeout        = imageTimeout
    setTimeout t i = i { imageTimeout = t }


instance Default Image where
    def = Image { imageToken   = ""
                , imageUrl     = ""
                , imageFields  = Nothing
                , imageTimeout = Nothing
                }


instance Request Image where
    toReq r = Req { reqApi     = "http://api.diffbot.com/v2/image"
                  , reqToken   = imageToken r
                  , reqUrl     = imageUrl r
                  , reqContent = Nothing
                  , reqQuery   = fieldsQuery r ++ timeoutQuery r
                  }


mkImage :: String -> String -> Image
mkImage token url = def { imageToken = token
                        , imageUrl   = url
                        }
