diffbot
=======

Simple client for the [Diffbot](http://diffbot.com) API.

# Installation

The easiest way to install the package and its dependencies is to use
the `cabal` command line tool. The
[Cabal-Install](http://www.haskell.org/haskellwiki/Cabal-Install) page
explains how to use `cabal`.

To install the package enter the following commands:

```
$ git clone https://github.com/tymmym/diffbot.git
$ cd diffbot
diffbot $ cabal install
```

You can also generate library documentation from annotated source code
using [Haddock](http://www.haskell.org/haddock):

```
diffbot $ cabal haddock
```

Alternatively you can read it
[online](http://tymmym.github.io/diffbot/doc/index.html).

# Usage

## Automatic APIs

Diffbot uses computer vision, natural language processing and machine
learning to automatically recognize and structure specific page-types.

To use the Automatic API, call `diffbot` function with following
arguments:

Argument | Description
---------|----------------
token    | Developer token
url      | URL to process
request  | API settings

Here is the full example with default request to the Article API:

```haskell
import Diffbot

main = do
    let token = "11111111111111111111111111111111"
        url   = "http://blog.diffbot.com/diffbots-new-product-api-teaches-robots-to-shop-online/"
    resp <- diffbot token url defArticle
    print resp
```

This code will print information about the primary article content on
the submitted page:

```
Just fromList [("author",String "John Davi"),("title",String "Diffbot\8217s New Product API Teaches Robots to Shop Online"),...
```

You can extract values from response with a parser using `parse`,
`parseEither` or, in this example, `parseMaybe` from
[aeson](http://hackage.haskell.org/package/aeson) package:

```haskell
getInfo :: Object -> Maybe String
getInfo resp = flip parseMaybe resp $ \obj -> do
    author <- obj .: "author"
    title  <- obj .: "title"
    return $ title ++ ", by " ++ author
```

You can use the same `diffbot` function to send requests to other
Automatic APIs (Frontpage, Product, Image and Page Classifier), e.g.:

```haskell
diffbot token url . setTimeout 15000 $ defFrontPage { frontPageAll = True }
```

## Custom API

You can also simply create requests to your [Custom
API](http://diffbot.com/products/custom). Just implement an instance
for the `Request` class. Look at the
[Article API sources](http://tymmym.github.io/diffbot/doc/src/Diffbot-Article.html#Article)
 for the reference.

## Crawlbot API

Crawlbot allows you to apply either Automatic APIs or your own Custom
API to intelligently extract an entire site.

To create a new crawl you should use `crawlbot` function:

```haskell
import Diffbot
import Diffbot.Crawlbot

main = do
    let token = "11111111111111111111111111111111"
        crawl = defaultCrawl "sampleDiffbotCrawl" ["http://blog.diffbot.com"]
    resp <- crawlbot token $ Create crawl
    print resp
```

You also can view, pause, restart or delete crawls.

# Details

Please consult [library
documentation](http://tymmym.github.io/diffbot/doc/index.html) for
additional information.
