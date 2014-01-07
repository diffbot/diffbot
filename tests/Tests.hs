{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Exception as E
import           Data.Maybe

import           Data.Default
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit hiding (Test, path)
import           Network.HTTP.Types.Status

import Diffbot


main :: IO ()
main = defaultMain tests


tests :: [Test]
tests = [ testCase "getIsJust" getIsJust
        , testCase "postPlainIsJust" postPlainIsJust
        , testCase "postHtmlIsJust" postHtmlIsJust
        , testCase "emptyToken" emptyToken
        ]


token, url :: String
token = "11111111111111111111111111111111"
url   = "http://blog.diffbot.com/diffbots-new-product-api-teaches-robots-to-shop-online/"


getIsJust :: Assertion
getIsJust = do
    resp <- diffbot $ mkRequest token url
    assertBool "Nothing" $ isJust resp


postPlainIsJust :: Assertion
postPlainIsJust = do
    let req = mkRequest token url
    -- Please note that the 'url' parameter is still required, and
    -- will be used to resolve any relative links contained in the
    -- markup.

    resp <- diffbot req { requestMethod = Post TextPlain "Diffbot\8217s human wranglers are proud today to announce the release of our newest product: an API for\8230 products!" }
    assertBool "Nothing" $ isJust resp


postHtmlIsJust :: Assertion
postHtmlIsJust = do
    let req = mkRequest token url
    resp <- diffbot req { requestMethod = Post TextHtml "<div><p>Diffbot&rsquo;s human wranglers are proud today to announce the release of our newest product: an API for&hellip; products!</p>" }
    assertBool "Nothing" $ isJust resp


emptyToken :: Assertion
emptyToken = do
    resp <- diffbot def
    assertBool "Nothing" $ isJust resp
    `E.catch` (\(StatusCodeException s _ _) ->
                   assertBool "Another exception" $ statusCode s == 401)
