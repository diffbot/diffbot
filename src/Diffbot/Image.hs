module Diffbot.Image where

import Data.Maybe

import Diffbot.Types


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


instance Request Image where
    toReq r = Req { reqApi     = "http://api.diffbot.com/v2/image"
                  , reqContent = Nothing
                  , reqQuery   = catMaybes [ fieldsQuery r
                                           , timeoutQuery r
                                           ]
                  }


defImage :: Image
defImage = Image { imageFields  = Nothing
                 , imageTimeout = Nothing
                 }
