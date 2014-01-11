module Product where

import Types

-- | Analyzes a shopping or e-commerce product page and returns
-- information on the product.
data Product = Product
    { productToken   :: String
    , productUrl     :: String
    , productFields  :: Maybe String
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
                  , reqToken   = productToken r
                  , reqUrl     = productUrl r
                  , reqContent = Nothing
                  , reqQuery   = fieldsQuery r ++ timeoutQuery r
                  }


mkProduct :: String -> String -> Product
mkProduct token url = Product { productToken   = token
                              , productUrl     = url
                              , productFields  = Nothing
                              , productTimeout = Nothing
                              }
