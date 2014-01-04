module Main where

import           Data.Maybe

import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit hiding (Test, path)

import Diffbot



main :: IO ()
main = defaultMain tests


tests :: [Test]
tests = [ testCase "responceIsJust" responceIsJust ]


responceIsJust :: Assertion
responceIsJust = do
    let token = "11111111111111111111111111111111"
        url   = "http://blog.diffbot.com/diffbots-new-product-api-teaches-robots-to-shop-online/"
    resp <- diffbot $ mkRequest token url
    print resp
    assertBool "Nothing" $ isJust resp
