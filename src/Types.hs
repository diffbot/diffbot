{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Types where

import           Control.Applicative
import qualified Data.ByteString.Lazy as BL


-- | All information on how to connect to a Diffbot and what should be
-- sent in the request.
class Request a where
    toReq :: a -> Req


data Req = Req
    { reqApi     :: String
    , reqContent :: Maybe Content
    , reqQuery   :: [(String, Maybe String)]
    }


appendQuery :: [(String, Maybe String)] -> Req -> Req
appendQuery query req = req { reqQuery = query ++ reqQuery req }


mkQuery :: String -> Maybe String -> Maybe (String, Maybe String)
mkQuery k v = do
    v' <- v
    return (k, Just v')


mkQueryBool :: String -> Bool -> Maybe (String, Maybe String)
mkQueryBool k b = if b then Just (k, Nothing) else Nothing


mkQueryFalse :: String -> Bool -> Maybe (String, Maybe String)
mkQueryFalse k b = if b then Nothing else Just (k, Just "0")


-- | Used to control which fields are returned by the API.
class Fields a where
    fields :: a -> Maybe String
    setFields :: Maybe String -> a -> a
    fieldsQuery :: a -> Maybe (String, Maybe String)
    fieldsQuery a = mkQuery "fields" $ fields a


class Post a where
    content :: a -> Maybe Content
    setContent :: Maybe Content -> a -> a


class Timeout a where
    timeout :: a -> Maybe Int
    setTimeout :: Maybe Int -> a -> a
    timeoutQuery :: a -> Maybe (String, Maybe String)
    timeoutQuery a = mkQuery "timeout" $ show <$> timeout a


data Content = Content
    { contentType :: ContentType
    -- ^ Type of content.
    , contentData :: BL.ByteString
    -- ^ Content to analyze.
    }


data ContentType = TextPlain
                 | TextHtml


-- strange behavior: the server returns error with corret "text/plain"
-- content-type
instance Show ContentType where
    show TextPlain = "text-plain"
    show TextHtml  = "text-html"
