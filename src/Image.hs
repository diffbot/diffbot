module Image where

import Data.Default
import Data.Maybe

import Types


-- | Analyzes a web page and returns its primary image(s).
data Image = Image
    { imageFields  :: Maybe String
    , imageTimeout :: Maybe Int
    }


instance Fields Image where
    fields        = imageFields
    setFields f i = i { imageFields = f }


instance Timeout Image where
    timeout        = imageTimeout
    setTimeout t i = i { imageTimeout = t }


instance Default Image where
    def = Image { imageFields  = Nothing
                , imageTimeout = Nothing
                }


instance Request Image where
    toReq r = Req { reqApi     = "http://api.diffbot.com/v2/image"
                  , reqContent = Nothing
                  , reqQuery   = catMaybes [ fieldsQuery r
                                           , timeoutQuery r
                                           ]
                  }


mkImage :: Image
mkImage = def
