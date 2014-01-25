module Diffbot.Product where

import Data.Maybe

import Diffbot.Types

-- | Analyzes a shopping or e-commerce product page and returns
-- information on the product.
data Product = Product
    { productFields  :: Maybe String
    , productTimeout :: Maybe Int
    }


instance Fields Product where
    fields        = productFields
    setFields f p = p { productFields = f }


instance Timeout Product where
    timeout        = productTimeout
    setTimeout t p = p { productTimeout = t }


instance Request Product where
    toReq r = Req { reqApi     = "http://api.diffbot.com/v2/product"
                  , reqContent = Nothing
                  , reqQuery   = catMaybes [ fieldsQuery r
                                           , timeoutQuery r
                                           ]
                  }


defProduct :: Product
defProduct = Product { productFields  = Nothing
                     , productTimeout = Nothing
                     }
